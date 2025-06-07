#!/usr/bin/env swift

import SwiftUI
import AppKit
import Foundation

// MARK: - FlowLoom Chat Model
@MainActor
class FlowLoomChat: ObservableObject {
    @Published var messages: [ChatMessage] = []
    @Published var currentInput = ""
    @Published var isProcessing = false
    
    init() {
        messages.append(ChatMessage(role: .assistant, content: "Hello! I'm FlowLoom, your AI Development Assistant. How can I help you today?"))
    }
    
    func sendMessage() {
        guard !currentInput.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty else { return }
        
        let userMessage = currentInput
        messages.append(ChatMessage(role: .user, content: userMessage))
        currentInput = ""
        isProcessing = true
        
        // Simulate API call - replace with actual Claude Code integration
        Task {
            // In real implementation, this would shell out to claude or use API
            let response = await processWithClaude(userMessage)
            await MainActor.run {
                messages.append(ChatMessage(role: .assistant, content: response))
                isProcessing = false
            }
        }
    }
    
    func processWithClaude(_ input: String) async -> String {
        // For now, simulate response. In real version:
        // 1. Shell out to 'claude' CLI command
        // 2. Or integrate with FlowLoom's command system
        // 3. Or use Anthropic API directly
        
        try? await Task.sleep(nanoseconds: 1_000_000_000) // 1 second delay
        
        // Detect FlowLoom commands
        if input.lowercased().contains("plan") {
            return "I'll help you with planning. Let me run FlowLoom's planning mode...\n\n[Would execute: flowloom plan:create]"
        } else if input.lowercased().contains("sync") {
            return "Running sync command...\n\n[Would execute: flowloom sync]"
        }
        
        return "I'm FlowLoom running in UI mode. In the full version, I would process: '\(input)' through Claude Code."
    }
}

// MARK: - Chat Message Model
struct ChatMessage: Identifiable {
    let id = UUID()
    let role: Role
    let content: String
    let timestamp = Date()
    
    enum Role {
        case user, assistant
    }
}

// MARK: - Main Chat View
struct FlowLoomChatView: View {
    @StateObject private var chat = FlowLoomChat()
    @FocusState private var inputFocused: Bool
    
    var body: some View {
        VStack(spacing: 0) {
            // Header
            HStack {
                Image(systemName: "flowchart.fill")
                    .foregroundColor(.accentColor)
                Text("FlowLoom AI Assistant")
                    .font(.headline)
                Spacer()
                Text("⌘K for commands")
                    .font(.caption)
                    .foregroundColor(.secondary)
            }
            .padding()
            .background(Color(NSColor.windowBackgroundColor))
            
            Divider()
            
            // Chat messages
            ScrollViewReader { proxy in
                ScrollView {
                    LazyVStack(alignment: .leading, spacing: 12) {
                        ForEach(chat.messages) { message in
                            MessageBubble(message: message)
                                .id(message.id)
                        }
                        
                        if chat.isProcessing {
                            HStack {
                                ProgressView()
                                    .scaleEffect(0.8)
                                Text("FlowLoom is thinking...")
                                    .font(.caption)
                                    .foregroundColor(.secondary)
                            }
                            .padding(.horizontal)
                        }
                    }
                    .padding()
                }
                .onChange(of: chat.messages.count) { _ in
                    withAnimation {
                        proxy.scrollTo(chat.messages.last?.id, anchor: .bottom)
                    }
                }
            }
            
            Divider()
            
            // Input area
            HStack(spacing: 12) {
                TextField("Ask FlowLoom anything...", text: $chat.currentInput, axis: .vertical)
                    .textFieldStyle(.plain)
                    .lineLimit(1...5)
                    .onSubmit {
                        if !NSEvent.modifierFlags.contains(.shift) {
                            chat.sendMessage()
                        }
                    }
                    .focused($inputFocused)
                
                Button(action: chat.sendMessage) {
                    Image(systemName: "arrow.up.circle.fill")
                        .font(.title2)
                }
                .buttonStyle(.plain)
                .disabled(chat.currentInput.isEmpty || chat.isProcessing)
            }
            .padding()
            .background(Color(NSColor.controlBackgroundColor))
        }
        .frame(minWidth: 600, minHeight: 400)
        .onAppear {
            inputFocused = true
        }
    }
}

// MARK: - Message Bubble Component
struct MessageBubble: View {
    let message: ChatMessage
    
    var body: some View {
        HStack(alignment: .top, spacing: 12) {
            // Avatar
            Image(systemName: message.role == .user ? "person.circle.fill" : "cpu.fill")
                .font(.title2)
                .foregroundColor(message.role == .user ? .blue : .green)
            
            VStack(alignment: .leading, spacing: 4) {
                // Role and timestamp
                HStack {
                    Text(message.role == .user ? "You" : "FlowLoom")
                        .font(.caption)
                        .fontWeight(.semibold)
                    Text(message.timestamp, style: .time)
                        .font(.caption)
                        .foregroundColor(.secondary)
                }
                
                // Message content
                Text(message.content)
                    .textSelection(.enabled)
                    .font(.body)
                
                // If message contains code or commands, style differently
                if message.content.contains("[Would execute:") {
                    Text(message.content.components(separatedBy: "[Would execute:").last?.dropLast() ?? "")
                        .font(.system(.caption, design: .monospaced))
                        .padding(8)
                        .background(Color.black.opacity(0.05))
                        .cornerRadius(4)
                }
            }
            
            Spacer()
        }
        .padding(.horizontal)
    }
}

// MARK: - App Delegate
class AppDelegate: NSObject, NSApplicationDelegate {
    var window: NSWindow!
    
    func applicationDidFinishLaunching(_ notification: Notification) {
        let contentView = FlowLoomChatView()
        
        window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 800, height: 600),
            styleMask: [.titled, .closable, .miniaturizable, .resizable],
            backing: .buffered,
            defer: false
        )
        
        window.center()
        window.title = "FlowLoom"
        window.contentView = NSHostingView(rootView: contentView)
        window.makeKeyAndOrderFront(nil)
    }
    
    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool {
        true
    }
}

// MARK: - Main App Entry
let app = NSApplication.shared
let delegate = AppDelegate()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.activate(ignoringOtherApps: true)
app.run()