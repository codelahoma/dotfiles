#!/usr/bin/env swift

import Foundation
import Combine

// MARK: - FlowLoom File-Based Bridge
// Communicates with FlowLoom through file system (memory.json and command files)

class FlowLoomFileBridge: FlowLoomBridgeProtocol {
    
    // File paths
    private let memoryPath: URL
    private let commandPath: URL
    private let responsePath: URL
    private let projectRoot: URL
    
    // Session tracking
    private let sessionId = UUID().uuidString
    private var commandCounter = 0
    
    // File monitoring
    private var fileMonitor: DispatchSourceFileSystemObject?
    private var responseWaitTime: TimeInterval = 10.0 // Max wait time for response
    
    init(projectRoot: String = FileManager.default.currentDirectoryPath) {
        self.projectRoot = URL(fileURLWithPath: projectRoot)
        self.memoryPath = self.projectRoot.appendingPathComponent("memory.json")
        
        // Create .flowloom directory for communication
        let flowloomDir = self.projectRoot.appendingPathComponent(".flowloom")
        try? FileManager.default.createDirectory(at: flowloomDir, withIntermediateDirectories: true)
        
        self.commandPath = flowloomDir.appendingPathComponent("ui-command.json")
        self.responsePath = flowloomDir.appendingPathComponent("ui-response.json")
    }
    
    // MARK: - Protocol Implementation
    
    func execute(_ command: String, args: [String]) async throws -> FlowLoomResponse {
        let commandId = generateCommandId()
        
        // Write command to file
        let commandData = FlowLoomFileCommand(
            id: commandId,
            sessionId: sessionId,
            command: command,
            args: args,
            timestamp: Date(),
            source: "ui"
        )
        
        try writeCommand(commandData)
        
        // Wait for response
        let response = try await waitForResponse(commandId: commandId)
        
        return response
    }
    
    func interpret(_ naturalLanguage: String) async throws -> FlowLoomResponse {
        // For natural language, we'll write to memory.json as an interaction
        let interaction = createMemoryInteraction(naturalLanguage)
        try appendToMemory(interaction)
        
        // Then execute a special interpret command
        return try await execute("interpret", args: [naturalLanguage])
    }
    
    func stream(_ input: String, handler: @escaping (FlowLoomChunk) -> Void) -> AnyCancellable {
        // For streaming, we'll monitor a stream file
        let streamPath = projectRoot.appendingPathComponent(".flowloom/ui-stream.txt")
        
        // Clear stream file
        try? "".write(to: streamPath, atomically: true, encoding: .utf8)
        
        // Start monitoring
        let subject = PassthroughSubject<FlowLoomChunk, Never>()
        
        let cancellable = subject.sink { chunk in
            handler(chunk)
        }
        
        // Write stream request
        Task {
            do {
                try await execute("stream", args: [input])
            } catch {
                subject.send(FlowLoomChunk(
                    content: "Stream error: \(error)",
                    target: .conversation,
                    format: .error,
                    isComplete: true
                ))
            }
        }
        
        // Monitor stream file
        monitorStreamFile(streamPath) { content in
            let chunk = FlowLoomChunk(
                content: content,
                target: .conversation,
                format: .plain,
                isComplete: false
            )
            subject.send(chunk)
        }
        
        return cancellable
    }
    
    // MARK: - File Operations
    
    private func writeCommand(_ command: FlowLoomFileCommand) throws {
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
        encoder.dateEncodingStrategy = .iso8601
        
        let data = try encoder.encode(command)
        try data.write(to: commandPath)
        
        // Also append to memory for tracking
        let memoryEntry = [
            "type": "ui_command",
            "command": command.command,
            "args": command.args,
            "timestamp": ISO8601DateFormatter().string(from: command.timestamp),
            "session": command.sessionId,
            "id": command.id
        ] as [String : Any]
        
        try appendToMemory(memoryEntry)
    }
    
    private func waitForResponse(commandId: String) async throws -> FlowLoomResponse {
        let startTime = Date()
        
        // Poll for response file
        while Date().timeIntervalSince(startTime) < responseWaitTime {
            if let response = try? readResponse(for: commandId) {
                // Clean up response file
                try? FileManager.default.removeItem(at: responsePath)
                return response
            }
            
            // Check memory.json for response
            if let memoryResponse = try? readMemoryResponse(for: commandId) {
                return memoryResponse
            }
            
            // Wait a bit before next check
            try await Task.sleep(nanoseconds: 100_000_000) // 0.1 seconds
        }
        
        throw FlowLoomError.timeout(command: commandId)
    }
    
    private func readResponse(for commandId: String) throws -> FlowLoomResponse {
        let data = try Data(contentsOf: responsePath)
        let fileResponse = try JSONDecoder().decode(FlowLoomFileResponse.self, from: data)
        
        // Verify this response is for our command
        guard fileResponse.commandId == commandId else {
            throw FlowLoomError.invalidResponse
        }
        
        return FlowLoomResponse(
            presentation: fileResponse.presentation,
            conversation: fileResponse.conversation,
            format: parseFormat(fileResponse.format),
            metadata: FlowLoomResponse.ResponseMetadata(
                commandExecuted: fileResponse.command,
                executionTime: fileResponse.executionTime,
                sessionId: sessionId,
                planContext: fileResponse.planContext
            )
        )
    }
    
    private func readMemoryResponse(for commandId: String) throws -> FlowLoomResponse? {
        guard let memoryData = try? Data(contentsOf: memoryPath),
              let memory = try? JSONSerialization.jsonObject(with: memoryData) as? [String: Any],
              let entries = memory["entries"] as? [[String: Any]] else {
            return nil
        }
        
        // Look for response in recent memory entries
        for entry in entries.reversed() {
            if let type = entry["type"] as? String,
               type == "ui_response",
               let responseCommandId = entry["commandId"] as? String,
               responseCommandId == commandId {
                
                return FlowLoomResponse(
                    presentation: entry["presentation"] as? String,
                    conversation: entry["conversation"] as? String,
                    format: parseFormat(entry["format"] as? String ?? "plain"),
                    metadata: FlowLoomResponse.ResponseMetadata(
                        commandExecuted: entry["command"] as? String,
                        executionTime: entry["executionTime"] as? TimeInterval ?? 0,
                        sessionId: sessionId,
                        planContext: entry["planContext"] as? String
                    )
                )
            }
        }
        
        return nil
    }
    
    private func appendToMemory(_ entry: [String: Any]) throws {
        var memory: [String: Any] = [:]
        
        // Read existing memory
        if let data = try? Data(contentsOf: memoryPath),
           let existing = try? JSONSerialization.jsonObject(with: data) as? [String: Any] {
            memory = existing
        }
        
        // Ensure entries array exists
        var entries = memory["entries"] as? [[String: Any]] ?? []
        entries.append(entry)
        memory["entries"] = entries
        memory["lastUpdated"] = ISO8601DateFormatter().string(from: Date())
        
        // Write back
        let data = try JSONSerialization.data(withJSONObject: memory, options: .prettyPrinted)
        try data.write(to: memoryPath)
    }
    
    private func createMemoryInteraction(_ input: String) -> [String: Any] {
        return [
            "type": "ui_interaction",
            "input": input,
            "timestamp": ISO8601DateFormatter().string(from: Date()),
            "session": sessionId,
            "source": "flowloom_ui"
        ]
    }
    
    private func monitorStreamFile(_ path: URL, handler: @escaping (String) -> Void) {
        // Simple file monitoring (in production, use FSEvents or similar)
        DispatchQueue.global().async {
            var lastContent = ""
            
            while true {
                if let content = try? String(contentsOf: path),
                   content != lastContent {
                    let newContent = String(content.dropFirst(lastContent.count))
                    if !newContent.isEmpty {
                        handler(newContent)
                        lastContent = content
                    }
                }
                Thread.sleep(forTimeInterval: 0.1)
            }
        }
    }
    
    private func generateCommandId() -> String {
        commandCounter += 1
        return "\(sessionId)-\(commandCounter)"
    }
    
    private func parseFormat(_ format: String) -> ContentFormat {
        switch format.lowercased() {
        case "markdown": return .markdown
        case "plain": return .plain
        case "error": return .error
        case let str where str.hasPrefix("code:"):
            return .code(language: String(str.dropFirst(5)))
        default:
            return .plain
        }
    }
}

// MARK: - File-Based Data Structures

struct FlowLoomFileCommand: Codable {
    let id: String
    let sessionId: String
    let command: String
    let args: [String]
    let timestamp: Date
    let source: String
}

struct FlowLoomFileResponse: Codable {
    let commandId: String
    let command: String?
    let presentation: String?
    let conversation: String?
    let format: String
    let executionTime: TimeInterval
    let planContext: String?
    let timestamp: Date
}

// MARK: - FlowLoom File Watcher

class FlowLoomFileWatcher {
    private let commandPath: URL
    private var fileHandle: FileHandle?
    private var source: DispatchSourceFileSystemObject?
    
    init(projectRoot: String) {
        let root = URL(fileURLWithPath: projectRoot)
        self.commandPath = root.appendingPathComponent(".flowloom/ui-command.json")
    }
    
    func startWatching(handler: @escaping (FlowLoomFileCommand) -> Void) {
        guard let fileHandle = try? FileHandle(forReadingFrom: commandPath) else {
            print("Failed to open command file for watching")
            return
        }
        
        self.fileHandle = fileHandle
        
        let source = DispatchSource.makeFileSystemObjectSource(
            fileDescriptor: fileHandle.fileDescriptor,
            eventMask: .write,
            queue: .main
        )
        
        source.setEventHandler { [weak self] in
            self?.handleFileChange(handler: handler)
        }
        
        source.setCancelHandler { [weak self] in
            self?.fileHandle?.closeFile()
        }
        
        self.source = source
        source.resume()
    }
    
    private func handleFileChange(handler: @escaping (FlowLoomFileCommand) -> Void) {
        guard let data = try? Data(contentsOf: commandPath),
              let command = try? JSONDecoder().decode(FlowLoomFileCommand.self, from: data) else {
            return
        }
        
        handler(command)
    }
    
    func stopWatching() {
        source?.cancel()
        source = nil
    }
}

// MARK: - Simple CLI for Testing

struct FlowLoomCLI {
    static func simulateFlowLoomResponse(to command: FlowLoomFileCommand) {
        // This simulates what FlowLoom would do when it sees a command
        let response = FlowLoomFileResponse(
            commandId: command.id,
            command: command.command,
            presentation: "# Response to: \(command.command)\n\nThis is a simulated response.",
            conversation: "Executed \(command.command) with args: \(command.args.joined(separator: ", "))",
            format: "markdown",
            executionTime: 0.5,
            planContext: nil,
            timestamp: Date()
        )
        
        // Write response
        let responsePath = URL(fileURLWithPath: FileManager.default.currentDirectoryPath)
            .appendingPathComponent(".flowloom/ui-response.json")
        
        if let data = try? JSONEncoder().encode(response) {
            try? data.write(to: responsePath)
        }
    }
}

// MARK: - Errors

enum FlowLoomError: LocalizedError {
    case timeout(command: String)
    case invalidResponse
    case bridgeNotAvailable
    
    var errorDescription: String? {
        switch self {
        case .timeout(let command):
            return "Timeout waiting for response to command: \(command)"
        case .invalidResponse:
            return "Invalid response format"
        case .bridgeNotAvailable:
            return "FlowLoom bridge not available"
        }
    }
}