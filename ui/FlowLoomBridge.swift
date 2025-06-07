#!/usr/bin/env swift

import Foundation
import Combine

// MARK: - FlowLoom Bridge Protocol

protocol FlowLoomBridgeProtocol {
    func execute(_ command: String, args: [String]) async throws -> FlowLoomResponse
    func interpret(_ naturalLanguage: String) async throws -> FlowLoomResponse
    func stream(_ input: String, handler: @escaping (FlowLoomChunk) -> Void) -> AnyCancellable
}

// MARK: - Response Types

struct FlowLoomChunk {
    let content: String
    let target: PaneTarget
    let format: ContentFormat
    let isComplete: Bool
    
    enum PaneTarget {
        case presentation
        case conversation
        case both
    }
}

struct FlowLoomResponse {
    let presentation: String?
    let conversation: String?
    let format: ContentFormat
    let metadata: ResponseMetadata
    
    struct ResponseMetadata {
        let commandExecuted: String?
        let executionTime: TimeInterval
        let sessionId: String
        let planContext: String?
    }
}

enum ContentFormat {
    case plain
    case markdown
    case code(language: String)
    case diagram(type: DiagramType)
    case table(headers: [String])
    case error
    
    enum DiagramType {
        case mermaid
        case graphviz
        case ascii
    }
}

// MARK: - FlowLoom Bridge Implementation

class FlowLoomBridge: FlowLoomBridgeProtocol {
    private let processQueue = DispatchQueue(label: "com.flowloom.bridge", qos: .userInitiated)
    private var activeProcess: Process?
    private let sessionId = UUID().uuidString
    
    // Paths
    private let flowloomPath: String
    private let claudePath = "/usr/local/bin/claude"
    
    init(flowloomPath: String = "/usr/local/bin/flowloom") {
        self.flowloomPath = flowloomPath
    }
    
    func execute(_ command: String, args: [String] = []) async throws -> FlowLoomResponse {
        let startTime = Date()
        
        return try await withCheckedThrowingContinuation { continuation in
            processQueue.async {
                do {
                    let result = try self.runFlowLoomCommand(command, args: args)
                    let response = self.parseFlowLoomOutput(result, command: command, startTime: startTime)
                    continuation.resume(returning: response)
                } catch {
                    continuation.resume(throwing: error)
                }
            }
        }
    }
    
    func interpret(_ naturalLanguage: String) async throws -> FlowLoomResponse {
        // First, use FlowLoom to interpret the natural language
        let interpretation = try await execute("interpret", args: [naturalLanguage])
        
        // If FlowLoom recognized a command, execute it
        if let command = interpretation.metadata.commandExecuted {
            return try await execute(command, args: [])
        }
        
        // Otherwise, pass through to Claude for general assistance
        return try await claudeQuery(naturalLanguage)
    }
    
    func stream(_ input: String, handler: @escaping (FlowLoomChunk) -> Void) -> AnyCancellable {
        let subject = PassthroughSubject<FlowLoomChunk, Never>()
        
        let cancellable = subject.sink { chunk in
            handler(chunk)
        }
        
        processQueue.async {
            // Set up streaming process
            let process = Process()
            process.executableURL = URL(fileURLWithPath: self.claudePath)
            process.arguments = ["--stream", input]
            
            let pipe = Pipe()
            process.standardOutput = pipe
            
            let handle = pipe.fileHandleForReading
            handle.readabilityHandler = { fileHandle in
                let data = fileHandle.availableData
                guard !data.isEmpty else {
                    subject.send(FlowLoomChunk(
                        content: "",
                        target: .conversation,
                        format: .plain,
                        isComplete: true
                    ))
                    subject.send(completion: .finished)
                    return
                }
                
                if let output = String(data: data, encoding: .utf8) {
                    let chunk = self.parseStreamChunk(output)
                    subject.send(chunk)
                }
            }
            
            do {
                try process.run()
                self.activeProcess = process
            } catch {
                subject.send(FlowLoomChunk(
                    content: "Error: \(error.localizedDescription)",
                    target: .conversation,
                    format: .error,
                    isComplete: true
                ))
                subject.send(completion: .finished)
            }
        }
        
        return cancellable
    }
    
    // MARK: - Private Methods
    
    private func runFlowLoomCommand(_ command: String, args: [String]) throws -> String {
        let process = Process()
        process.executableURL = URL(fileURLWithPath: flowloomPath)
        process.arguments = [command] + args
        
        let outputPipe = Pipe()
        let errorPipe = Pipe()
        process.standardOutput = outputPipe
        process.standardError = errorPipe
        
        // Set FlowLoom-specific environment
        var environment = ProcessInfo.processInfo.environment
        environment["FLOWLOOM_OUTPUT_FORMAT"] = "json"
        environment["FLOWLOOM_SESSION_ID"] = sessionId
        process.environment = environment
        
        try process.run()
        process.waitUntilExit()
        
        let outputData = outputPipe.fileHandleForReading.readDataToEndOfFile()
        let errorData = errorPipe.fileHandleForReading.readDataToEndOfFile()
        
        if process.terminationStatus != 0 {
            let errorMessage = String(data: errorData, encoding: .utf8) ?? "Unknown error"
            throw FlowLoomError.commandFailed(command: command, message: errorMessage)
        }
        
        return String(data: outputData, encoding: .utf8) ?? ""
    }
    
    private func parseFlowLoomOutput(_ output: String, command: String, startTime: Date) -> FlowLoomResponse {
        // Try to parse as JSON first
        if let data = output.data(using: .utf8),
           let json = try? JSONSerialization.jsonObject(with: data) as? [String: Any] {
            return parseJSONResponse(json, command: command, startTime: startTime)
        }
        
        // Fallback to content analysis
        return analyzeContent(output, command: command, startTime: startTime)
    }
    
    private func parseJSONResponse(_ json: [String: Any], command: String, startTime: Date) -> FlowLoomResponse {
        let presentation = json["presentation"] as? String
        let conversation = json["conversation"] as? String
        let formatStr = json["format"] as? String ?? "plain"
        let planContext = json["plan"] as? String
        
        let format = parseFormat(formatStr)
        
        let metadata = FlowLoomResponse.ResponseMetadata(
            commandExecuted: command,
            executionTime: Date().timeIntervalSince(startTime),
            sessionId: sessionId,
            planContext: planContext
        )
        
        return FlowLoomResponse(
            presentation: presentation,
            conversation: conversation,
            format: format,
            metadata: metadata
        )
    }
    
    private func analyzeContent(_ content: String, command: String, startTime: Date) -> FlowLoomResponse {
        // Smart content routing based on patterns
        var format = ContentFormat.plain
        var presentation: String?
        var conversation: String?
        
        // Detect code blocks
        if content.contains("```") {
            format = .markdown
            presentation = content
            conversation = "I've displayed the code in the presentation pane."
        }
        // Detect markdown headers
        else if content.contains("# ") || content.contains("## ") {
            format = .markdown
            presentation = content
            conversation = "I've formatted the response as markdown in the presentation pane."
        }
        // Detect tables
        else if content.contains("|") && content.contains("-|-") {
            format = .markdown
            presentation = content
            conversation = "I've displayed the table in the presentation pane."
        }
        // Short responses go to conversation
        else if content.count < 200 {
            conversation = content
        }
        // Long responses go to presentation
        else {
            presentation = content
            conversation = "I've displayed the detailed response in the presentation pane."
        }
        
        let metadata = FlowLoomResponse.ResponseMetadata(
            commandExecuted: command,
            executionTime: Date().timeIntervalSince(startTime),
            sessionId: sessionId,
            planContext: nil
        )
        
        return FlowLoomResponse(
            presentation: presentation,
            conversation: conversation,
            format: format,
            metadata: metadata
        )
    }
    
    private func parseStreamChunk(_ output: String) -> FlowLoomChunk {
        // Parse streaming output format
        // Expected format: "PANE:format:content" or just content
        
        if output.hasPrefix("PRESENTATION:") {
            let parts = output.dropFirst("PRESENTATION:".count).split(separator: ":", maxSplits: 1)
            let format = parseFormat(String(parts.first ?? "plain"))
            let content = String(parts.last ?? "")
            
            return FlowLoomChunk(
                content: content,
                target: .presentation,
                format: format,
                isComplete: false
            )
        } else if output.hasPrefix("CONVERSATION:") {
            let content = String(output.dropFirst("CONVERSATION:".count))
            return FlowLoomChunk(
                content: content,
                target: .conversation,
                format: .plain,
                isComplete: false
            )
        } else {
            // Default to conversation
            return FlowLoomChunk(
                content: output,
                target: .conversation,
                format: .plain,
                isComplete: false
            )
        }
    }
    
    private func parseFormat(_ formatStr: String) -> ContentFormat {
        switch formatStr.lowercased() {
        case "markdown", "md":
            return .markdown
        case "plain", "text":
            return .plain
        case "error":
            return .error
        case let str where str.hasPrefix("code:"):
            let language = String(str.dropFirst("code:".count))
            return .code(language: language)
        case let str where str.hasPrefix("diagram:"):
            let type = String(str.dropFirst("diagram:".count))
            return .diagram(type: parseDiagramType(type))
        case let str where str.hasPrefix("table:"):
            let headers = String(str.dropFirst("table:".count))
                .split(separator: ",")
                .map(String.init)
            return .table(headers: headers)
        default:
            return .plain
        }
    }
    
    private func parseDiagramType(_ type: String) -> ContentFormat.DiagramType {
        switch type.lowercased() {
        case "mermaid":
            return .mermaid
        case "graphviz", "dot":
            return .graphviz
        default:
            return .ascii
        }
    }
    
    private func claudeQuery(_ input: String) async throws -> FlowLoomResponse {
        // Direct Claude integration for natural language processing
        let startTime = Date()
        
        let process = Process()
        process.executableURL = URL(fileURLWithPath: claudePath)
        process.arguments = [input]
        
        let outputPipe = Pipe()
        process.standardOutput = outputPipe
        
        try process.run()
        process.waitUntilExit()
        
        let outputData = outputPipe.fileHandleForReading.readDataToEndOfFile()
        let output = String(data: outputData, encoding: .utf8) ?? ""
        
        return analyzeContent(output, command: "claude", startTime: startTime)
    }
}

// MARK: - Errors

enum FlowLoomError: LocalizedError {
    case commandFailed(command: String, message: String)
    case bridgeNotAvailable
    case invalidResponse
    
    var errorDescription: String? {
        switch self {
        case .commandFailed(let command, let message):
            return "FlowLoom command '\(command)' failed: \(message)"
        case .bridgeNotAvailable:
            return "FlowLoom bridge is not available"
        case .invalidResponse:
            return "Invalid response from FlowLoom"
        }
    }
}

// MARK: - Mock Implementation for Testing

class MockFlowLoomBridge: FlowLoomBridgeProtocol {
    func execute(_ command: String, args: [String]) async throws -> FlowLoomResponse {
        // Simulate different command responses
        switch command {
        case "plan:create":
            return FlowLoomResponse(
                presentation: """
                # Project Plan: \(args.first ?? "New Project")
                
                ## Overview
                This is a generated project plan.
                
                ## Phases
                1. Research and Analysis
                2. Architecture Design
                3. Implementation
                4. Testing and Validation
                5. Documentation
                
                ## Timeline
                Total estimated time: 4-6 weeks
                """,
                conversation: "I've created a project plan for '\(args.first ?? "your project")'.",
                format: .markdown,
                metadata: FlowLoomResponse.ResponseMetadata(
                    commandExecuted: command,
                    executionTime: 0.5,
                    sessionId: UUID().uuidString,
                    planContext: nil
                )
            )
            
        case "code:generate":
            return FlowLoomResponse(
                presentation: """
                ```swift
                // Generated by FlowLoom
                struct \(args.first ?? "MyStruct") {
                    var id: UUID = UUID()
                    var name: String
                    var timestamp: Date = Date()
                    
                    init(name: String) {
                        self.name = name
                    }
                }
                ```
                """,
                conversation: "I've generated the Swift code structure.",
                format: .code(language: "swift"),
                metadata: FlowLoomResponse.ResponseMetadata(
                    commandExecuted: command,
                    executionTime: 0.3,
                    sessionId: UUID().uuidString,
                    planContext: nil
                )
            )
            
        default:
            return FlowLoomResponse(
                presentation: nil,
                conversation: "Command '\(command)' executed successfully.",
                format: .plain,
                metadata: FlowLoomResponse.ResponseMetadata(
                    commandExecuted: command,
                    executionTime: 0.1,
                    sessionId: UUID().uuidString,
                    planContext: nil
                )
            )
        }
    }
    
    func interpret(_ naturalLanguage: String) async throws -> FlowLoomResponse {
        // Simple pattern matching for demo
        let lower = naturalLanguage.lowercased()
        
        if lower.contains("plan") {
            return try await execute("plan:create", args: [naturalLanguage])
        } else if lower.contains("code") || lower.contains("function") {
            return try await execute("code:generate", args: [naturalLanguage])
        } else {
            return FlowLoomResponse(
                presentation: nil,
                conversation: "I understand: '\(naturalLanguage)'. How can I help you with that?",
                format: .plain,
                metadata: FlowLoomResponse.ResponseMetadata(
                    commandExecuted: nil,
                    executionTime: 0.1,
                    sessionId: UUID().uuidString,
                    planContext: nil
                )
            )
        }
    }
    
    func stream(_ input: String, handler: @escaping (FlowLoomChunk) -> Void) -> AnyCancellable {
        let subject = PassthroughSubject<FlowLoomChunk, Never>()
        
        let cancellable = subject.sink { chunk in
            handler(chunk)
        }
        
        // Simulate streaming response
        Task {
            let words = input.split(separator: " ")
            for (index, word) in words.enumerated() {
                try? await Task.sleep(nanoseconds: 100_000_000) // 0.1s
                
                let chunk = FlowLoomChunk(
                    content: String(word) + " ",
                    target: .conversation,
                    format: .plain,
                    isComplete: index == words.count - 1
                )
                subject.send(chunk)
            }
            subject.send(completion: .finished)
        }
        
        return cancellable
    }
}