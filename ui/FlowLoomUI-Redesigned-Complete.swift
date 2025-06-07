#!/usr/bin/env swift

import SwiftUI
import AppKit
import WebKit

// Include all the component files
// In a real app, these would be imported as modules

// MARK: - Main App

@main
struct FlowLoomApp: App {
    @StateObject private var viewModel = FlowLoomViewModel()
    
    var body: some Scene {
        WindowGroup {
            FlowLoomMainView(viewModel: viewModel)
                .frame(minWidth: 800, minHeight: 600)
        }
        .windowStyle(.hiddenTitleBar)
        .commands {
            CommandGroup(replacing: .appInfo) {
                Button("About FlowLoom") {
                    showAboutWindow()
                }
            }
            
            CommandMenu("FlowLoom") {
                Button("Show Markdown Viewer") {
                    viewModel.showMarkdownViewer.toggle()
                }
                .keyboardShortcut("m", modifiers: [.command])
                
                Divider()
                
                Button("Clear Activity Log") {
                    viewModel.activityLog.removeAll()
                }
                .keyboardShortcut("k", modifiers: [.command, .shift])
            }
        }
    }
    
    private func showAboutWindow() {
        let aboutView = VStack(spacing: 20) {
            Image(systemName: "flowchart")
                .font(.system(size: 64))
                .foregroundColor(.accentColor)
            
            Text("FlowLoom")
                .font(.largeTitle)
                .fontWeight(.bold)
            
            Text("AI Development Assistant")
                .font(.title3)
                .foregroundColor(.secondary)
            
            Text("Version 1.0.0")
                .font(.caption)
            
            Divider()
                .frame(width: 200)
            
            Text("Enhanced with three-panel layout and dedicated markdown viewer")
                .multilineTextAlignment(.center)
                .frame(width: 300)
        }
        .padding(40)
        .frame(width: 400, height: 300)
        
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 400, height: 300),
            styleMask: [.titled, .closable],
            backing: .buffered,
            defer: false
        )
        window.title = "About FlowLoom"
        window.contentView = NSHostingView(rootView: aboutView)
        window.center()
        window.makeKeyAndOrderFront(nil)
    }
}

// MARK: - Main View

struct FlowLoomMainView: View {
    @ObservedObject var viewModel: FlowLoomViewModel
    @State private var presentationSplitRatio: CGFloat = 0.6
    
    var body: some View {
        ZStack {
            // Main content
            VStack(spacing: 0) {
                // Title bar
                FlowLoomTitleBar(viewModel: viewModel)
                
                Divider()
                
                // Main content area with presentation and conversation
                GeometryReader { geometry in
                    HStack(spacing: 0) {
                        // Presentation panel
                        PresentationPanel(viewModel: viewModel)
                            .frame(width: geometry.size.width * presentationSplitRatio)
                        
                        // Resizable divider
                        ResizableDivider(ratio: $presentationSplitRatio)
                        
                        // Conversation panel
                        ConversationPanel(viewModel: viewModel)
                    }
                }
                
                Divider()
                
                // New three-panel bottom
                BottomPanelView(viewModel: viewModel)
            }
            .background(Color(NSColor.windowBackgroundColor))
            
            // Markdown viewer overlay
            MarkdownSlideOutPanel(
                isShowing: $viewModel.showMarkdownViewer,
                markdownFile: $viewModel.selectedMarkdownFile
            )
        }
    }
}

// MARK: - Title Bar

struct FlowLoomTitleBar: View {
    @ObservedObject var viewModel: FlowLoomViewModel
    
    var body: some View {
        HStack(spacing: 16) {
            // Logo and title
            HStack(spacing: 8) {
                Image(systemName: "flowchart")
                    .font(.title2)
                    .foregroundColor(.accentColor)
                
                Text("FlowLoom")
                    .font(.title2)
                    .fontWeight(.semibold)
                
                Text("AI Development Assistant")
                    .font(.caption)
                    .foregroundColor(.secondary)
            }
            
            Spacer()
            
            // Status indicators
            HStack(spacing: 12) {
                // Connection status
                HStack(spacing: 4) {
                    Circle()
                        .fill(viewModel.connectionStatus.color)
                        .frame(width: 8, height: 8)
                    Text(viewModel.connectionStatus.displayText)
                        .font(.caption)
                }
                
                Divider()
                    .frame(height: 20)
                
                // Session info
                Text("Session: \(String(viewModel.sessionId.prefix(8)))")
                    .font(.caption)
                    .foregroundColor(.secondary)
                
                Divider()
                    .frame(height: 20)
                
                // Quick actions
                Button(action: { viewModel.showMarkdownViewer.toggle() }) {
                    Image(systemName: "doc.text")
                }
                .buttonStyle(.plain)
                .help("Toggle Markdown Viewer (⌘M)")
            }
        }
        .padding(.horizontal, 20)
        .padding(.vertical, 10)
        .background(Color(NSColor.windowBackgroundColor))
    }
}

// MARK: - Presentation Panel

struct PresentationPanel: View {
    @ObservedObject var viewModel: FlowLoomViewModel
    
    var body: some View {
        VStack(spacing: 0) {
            // Panel header
            HStack {
                Label("Presentation", systemImage: "tv")
                    .font(.caption)
                    .foregroundColor(.secondary)
                
                Spacer()
                
                Text(formatDescription(viewModel.presentationFormat))
                    .font(.caption)
                    .foregroundColor(.secondary)
                    .padding(.horizontal, 8)
                    .padding(.vertical, 2)
                    .background(Color.accentColor.opacity(0.2))
                    .cornerRadius(10)
            }
            .padding(.horizontal, 12)
            .padding(.vertical, 8)
            .background(Color(NSColor.controlBackgroundColor))
            
            // Content area
            if viewModel.presentationContent.isEmpty {
                EmptyPresentationView()
            } else {
                switch viewModel.presentationFormat {
                case .markdown:
                    WebContentView(content: viewModel.presentationContent, format: .markdown)
                case .code(let language):
                    WebContentView(content: viewModel.presentationContent, format: .code(language: language))
                case .diagram:
                    WebContentView(content: viewModel.presentationContent, format: .diagram)
                default:
                    ScrollView {
                        Text(viewModel.presentationContent)
                            .padding()
                            .frame(maxWidth: .infinity, alignment: .leading)
                    }
                }
            }
        }
    }
    
    private func formatDescription(_ format: FlowLoomMessage.ContentFormat) -> String {
        switch format {
        case .plain: return "Plain Text"
        case .markdown: return "Markdown"
        case .code(let lang): return "Code (\(lang))"
        case .diagram: return "Diagram"
        case .error: return "Error"
        }
    }
}

// MARK: - Conversation Panel

struct ConversationPanel: View {
    @ObservedObject var viewModel: FlowLoomViewModel
    
    var body: some View {
        VStack(spacing: 0) {
            // Panel header
            HStack {
                Label("Conversation", systemImage: "bubble.left.and.bubble.right")
                    .font(.caption)
                    .foregroundColor(.secondary)
                
                Spacer()
                
                Text("\(viewModel.conversationHistory.count) messages")
                    .font(.caption)
                    .foregroundColor(.secondary)
            }
            .padding(.horizontal, 12)
            .padding(.vertical, 8)
            .background(Color(NSColor.controlBackgroundColor))
            
            // Messages
            ScrollViewReader { proxy in
                ScrollView {
                    LazyVStack(alignment: .leading, spacing: 12) {
                        ForEach(viewModel.conversationHistory) { message in
                            MessageBubbleView(message: message)
                                .id(message.id)
                        }
                        
                        if viewModel.conversationHistory.isEmpty {
                            EmptyConversationView()
                                .frame(maxWidth: .infinity, maxHeight: .infinity)
                                .frame(minHeight: 200)
                        }
                    }
                    .padding()
                }
                .onChange(of: viewModel.conversationHistory.count) { _ in
                    if let last = viewModel.conversationHistory.last {
                        withAnimation {
                            proxy.scrollTo(last.id, anchor: .bottom)
                        }
                    }
                }
            }
        }
    }
}

// MARK: - Supporting Views

struct EmptyPresentationView: View {
    var body: some View {
        VStack(spacing: 20) {
            Image(systemName: "tv")
                .font(.system(size: 48))
                .foregroundColor(.secondary)
            
            Text("No presentation content")
                .font(.title3)
                .foregroundColor(.secondary)
            
            Text("Content will appear here when FlowLoom generates visual output")
                .font(.caption)
                .foregroundColor(.tertiary)
                .multilineTextAlignment(.center)
                .frame(maxWidth: 300)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }
}

struct EmptyConversationView: View {
    var body: some View {
        VStack(spacing: 20) {
            Image(systemName: "bubble.left.and.bubble.right")
                .font(.system(size: 48))
                .foregroundColor(.secondary)
            
            Text("Start a conversation")
                .font(.title3)
                .foregroundColor(.secondary)
            
            Text("Type a command or message in the input panel below")
                .font(.caption)
                .foregroundColor(.tertiary)
        }
    }
}

struct MessageBubbleView: View {
    let message: FlowLoomMessage
    
    var body: some View {
        HStack(alignment: .top, spacing: 8) {
            if message.type == .user {
                Spacer(minLength: 60)
            }
            
            VStack(alignment: message.type == .user ? .trailing : .leading, spacing: 4) {
                HStack(spacing: 4) {
                    Image(systemName: iconForMessageType(message.type))
                        .font(.caption2)
                    
                    Text(labelForMessageType(message.type))
                        .font(.caption2)
                        .fontWeight(.medium)
                    
                    Text(message.timestamp, style: .time)
                        .font(.caption2)
                }
                .foregroundColor(.secondary)
                
                Text(message.content)
                    .padding(.horizontal, 12)
                    .padding(.vertical, 8)
                    .background(backgroundForMessageType(message.type))
                    .cornerRadius(12)
                    .foregroundColor(message.type == .user ? .white : .primary)
            }
            
            if message.type != .user {
                Spacer(minLength: 60)
            }
        }
    }
    
    private func iconForMessageType(_ type: FlowLoomMessage.MessageType) -> String {
        switch type {
        case .user: return "person.circle"
        case .assistant: return "sparkles"
        case .system: return "gear"
        }
    }
    
    private func labelForMessageType(_ type: FlowLoomMessage.MessageType) -> String {
        switch type {
        case .user: return "You"
        case .assistant: return "FlowLoom"
        case .system: return "System"
        }
    }
    
    private func backgroundForMessageType(_ type: FlowLoomMessage.MessageType) -> Color {
        switch type {
        case .user: return .accentColor
        case .assistant: return Color(NSColor.controlBackgroundColor)
        case .system: return Color.orange.opacity(0.2)
        }
    }
}

struct ResizableDivider: View {
    @Binding var ratio: CGFloat
    @State private var isDragging = false
    
    var body: some View {
        Rectangle()
            .fill(Color(NSColor.separatorColor))
            .frame(width: 1)
            .overlay(
                Rectangle()
                    .fill(Color.clear)
                    .frame(width: 8)
                    .contentShape(Rectangle())
                    .cursor(.resizeLeftRight)
                    .gesture(
                        DragGesture()
                            .onChanged { value in
                                isDragging = true
                                let delta = value.translation.width / NSScreen.main!.frame.width
                                ratio = max(0.3, min(0.7, ratio + delta))
                            }
                            .onEnded { _ in
                                isDragging = false
                            }
                    )
            )
            .background(isDragging ? Color.accentColor.opacity(0.5) : Color.clear)
            .animation(.easeOut(duration: 0.1), value: isDragging)
    }
}

struct WebContentView: NSViewRepresentable {
    let content: String
    let format: FlowLoomMessage.ContentFormat
    
    func makeNSView(context: Context) -> WKWebView {
        let webView = WKWebView()
        webView.setValue(false, forKey: "drawsBackground")
        return webView
    }
    
    func updateNSView(_ webView: WKWebView, context: Context) {
        let html = generateHTML()
        webView.loadHTMLString(html, baseURL: nil)
    }
    
    private func generateHTML() -> String {
        // Generate appropriate HTML based on format
        let isDark = NSApp.effectiveAppearance.name == .darkAqua
        
        return """
        <!DOCTYPE html>
        <html>
        <head>
            <style>
                body {
                    font-family: -apple-system, sans-serif;
                    margin: 20px;
                    background: \(isDark ? "#1e1e1e" : "#ffffff");
                    color: \(isDark ? "#e0e0e0" : "#333333");
                }
            </style>
        </head>
        <body>
            \(content)
        </body>
        </html>
        """
    }
}

// MARK: - Cursor Extension

extension View {
    func cursor(_ cursor: NSCursor) -> some View {
        self.onHover { inside in
            if inside {
                cursor.push()
            } else {
                NSCursor.pop()
            }
        }
    }
}