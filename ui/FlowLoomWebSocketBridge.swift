#!/usr/bin/env swift

import Foundation
import Combine
import Network

// MARK: - WebSocket Bridge Implementation

class FlowLoomWebSocketBridge: FlowLoomBridgeProtocol {
    private let webSocketURL: URL
    private var webSocketTask: URLSessionWebSocketTask?
    private let session = URLSession.shared
    private let sessionId = UUID().uuidString
    
    private var cancellables = Set<AnyCancellable>()
    private let responseSubject = PassthroughSubject<FlowLoomResponse, Never>()
    private let chunkSubject = PassthroughSubject<FlowLoomChunk, Never>()
    
    init(serverURL: String = "ws://127.0.0.1:8891/ws") {
        guard let url = URL(string: serverURL) else {
            fatalError("Invalid WebSocket URL: \(serverURL)")
        }
        self.webSocketURL = url
        setupWebSocketConnection()
    }
    
    private func setupWebSocketConnection() {
        webSocketTask = session.webSocketTask(with: webSocketURL)
        webSocketTask?.resume()
        startListening()
    }
    
    private func startListening() {
        webSocketTask?.receive { [weak self] result in
            switch result {
            case .success(let message):
                self?.handleWebSocketMessage(message)
                self?.startListening() // Continue listening
                
            case .failure(let error):
                print("WebSocket error: \(error)")
                // Attempt to reconnect after a delay
                DispatchQueue.main.asyncAfter(deadline: .now() + 5) {
                    self?.setupWebSocketConnection()
                }
            }
        }
    }
    
    private func handleWebSocketMessage(_ message: URLSessionWebSocketTask.Message) {
        switch message {
        case .string(let text):
            handleTextMessage(text)
        case .data(let data):
            if let text = String(data: data, encoding: .utf8) {
                handleTextMessage(text)
            }
        @unknown default:
            break
        }
    }
    
    private func handleTextMessage(_ text: String) {
        guard let data = text.data(using: .utf8),
              let json = try? JSONSerialization.jsonObject(with: data) as? [String: Any] else {
            return
        }
        
        if let type = json["type"] as? String {
            switch type {
            case "memory_update":
                handleMemoryUpdate(json)
            case "command_response":
                handleCommandResponse(json)
            case "stream_chunk":
                handleStreamChunk(json)
            case "echo":
                handleEcho(json)
            default:
                break
            }
        }
    }
    
    private func handleMemoryUpdate(_ json: [String: Any]) {
        // Handle memory graph updates
        if let data = json["data"] {
            let response = FlowLoomResponse(
                presentation: formatMemoryData(data),
                conversation: nil,
                format: .markdown,
                metadata: FlowLoomResponse.ResponseMetadata(
                    commandExecuted: nil,
                    executionTime: 0,
                    sessionId: sessionId,
                    planContext: nil
                )
            )
            responseSubject.send(response)
        }
    }
    
    private func handleCommandResponse(_ json: [String: Any]) {
        let presentation = json["presentation"] as? String
        let conversation = json["conversation"] as? String
        let formatString = json["format"] as? String ?? "markdown"
        let executionTime = json["execution_time"] as? TimeInterval ?? 0
        let command = json["command"] as? String
        
        let format = parseContentFormat(formatString)
        let response = FlowLoomResponse(
            presentation: presentation,
            conversation: conversation,
            format: format,
            metadata: FlowLoomResponse.ResponseMetadata(
                commandExecuted: command,
                executionTime: executionTime,
                sessionId: sessionId,
                planContext: nil
            )
        )
        
        responseSubject.send(response)
    }
    
    private func handleStreamChunk(_ json: [String: Any]) {
        let content = json["content"] as? String ?? ""
        let targetString = json["target"] as? String ?? "conversation"
        let formatString = json["format"] as? String ?? "plain"
        let isComplete = json["is_complete"] as? Bool ?? false
        
        let target = parseTarget(targetString)
        let format = parseContentFormat(formatString)
        
        let chunk = FlowLoomChunk(
            content: content,
            target: target,
            format: format,
            isComplete: isComplete
        )
        
        chunkSubject.send(chunk)
    }
    
    private func handleEcho(_ json: [String: Any]) {
        // Handle echo responses for testing
        if let message = json["message"] as? String {
            let response = FlowLoomResponse(
                presentation: nil,
                conversation: "Echo: \(message)",
                format: .plain,
                metadata: FlowLoomResponse.ResponseMetadata(
                    commandExecuted: "echo",
                    executionTime: 0,
                    sessionId: sessionId,
                    planContext: nil
                )
            )
            responseSubject.send(response)
        }
    }
    
    private func sendWebSocketMessage(_ message: [String: Any]) {
        guard let data = try? JSONSerialization.data(withJSONObject: message),
              let text = String(data: data, encoding: .utf8) else {
            return
        }
        
        webSocketTask?.send(.string(text)) { error in
            if let error = error {
                print("WebSocket send error: \(error)")
            }
        }
    }
    
    // MARK: - FlowLoomBridgeProtocol Implementation
    
    func execute(_ command: String, args: [String] = []) async throws -> FlowLoomResponse {
        let message = [
            "type": "command",
            "command": command,
            "args": args,
            "session_id": sessionId,
            "timestamp": Date().timeIntervalSince1970
        ] as [String: Any]
        
        sendWebSocketMessage(message)
        
        // Wait for response
        return try await withCheckedThrowingContinuation { continuation in
            let cancellable = responseSubject
                .timeout(.seconds(10), scheduler: DispatchQueue.main)
                .sink(
                    receiveCompletion: { completion in
                        if case .failure(let error) = completion {
                            continuation.resume(throwing: error)
                        }
                    },
                    receiveValue: { response in
                        continuation.resume(returning: response)
                    }
                )
            
            cancellables.insert(cancellable)
        }
    }
    
    func interpret(_ naturalLanguage: String) async throws -> FlowLoomResponse {
        let message = [
            "type": "interpret",
            "text": naturalLanguage,
            "session_id": sessionId,
            "timestamp": Date().timeIntervalSince1970
        ] as [String: Any]
        
        sendWebSocketMessage(message)
        
        // Wait for response
        return try await withCheckedThrowingContinuation { continuation in
            let cancellable = responseSubject
                .timeout(.seconds(15), scheduler: DispatchQueue.main)
                .sink(
                    receiveCompletion: { completion in
                        if case .failure(let error) = completion {
                            continuation.resume(throwing: error)
                        }
                    },
                    receiveValue: { response in
                        continuation.resume(returning: response)
                    }
                )
            
            cancellables.insert(cancellable)
        }
    }
    
    func stream(_ input: String, handler: @escaping (FlowLoomChunk) -> Void) -> AnyCancellable {
        let message = [
            "type": "stream",
            "text": input,
            "session_id": sessionId,
            "timestamp": Date().timeIntervalSince1970
        ] as [String: Any]
        
        sendWebSocketMessage(message)
        
        return chunkSubject.sink { chunk in
            handler(chunk)
        }
    }
    
    // MARK: - Helper Methods
    
    private func formatMemoryData(_ data: Any) -> String {
        guard let jsonData = try? JSONSerialization.data(withJSONObject: data),
              let prettyJson = try? JSONSerialization.jsonObject(with: jsonData),
              let prettyData = try? JSONSerialization.data(withJSONObject: prettyJson, options: .prettyPrinted),
              let prettyString = String(data: prettyData, encoding: .utf8) else {
            return "# Memory Update\n\nReceived memory graph update"
        }
        
        return """
        # Memory Graph Update
        
        ```json
        \(prettyString)
        ```
        """
    }
    
    private func parseTarget(_ target: String) -> FlowLoomChunk.PaneTarget {
        switch target.lowercased() {
        case "presentation": return .presentation
        case "conversation": return .conversation
        case "both": return .both
        default: return .conversation
        }
    }
    
    private func parseContentFormat(_ format: String) -> ContentFormat {
        switch format.lowercased() {
        case "plain": return .plain
        case "markdown": return .markdown
        case "error": return .error
        default:
            if format.hasPrefix("code:") {
                let language = String(format.dropFirst(5))
                return .code(language: language)
            }
            return .markdown
        }
    }
    
    deinit {
        webSocketTask?.cancel()
    }
}