#!/usr/bin/env swift

import SwiftUI
import AppKit
import AVFoundation
import Speech
import WebKit
import Combine

// MARK: - Models

struct FlowLoomMessage: Identifiable {
    let id = UUID()
    let content: String
    let type: MessageType
    let timestamp = Date()
    let format: ContentFormat
    
    enum MessageType {
        case user, assistant, system
    }
    
    enum ContentFormat {
        case plain, markdown, code(language: String), diagram
    }
}

struct FlowLoomResponse {
    let presentation: String?
    let conversation: String?
    let format: FlowLoomMessage.ContentFormat
}

// MARK: - View Models

@MainActor
class FlowLoomViewModel: ObservableObject {
    @Published var userInput = ""
    @Published var conversationHistory: [FlowLoomMessage] = []
    @Published var presentationContent = ""
    @Published var presentationFormat: FlowLoomMessage.ContentFormat = .markdown
    @Published var isListening = false
    @Published var isProcessing = false
    @Published var voiceEnabled = true
    @Published var audioLevel: Float = 0.0
    
    private var speechRecognizer: SFSpeechRecognizer?
    private var recognitionRequest: SFSpeechAudioBufferRecognitionRequest?
    private var recognitionTask: SFSpeechRecognitionTask?
    private let audioEngine = AVAudioEngine()
    private let speechSynthesizer = AVSpeechSynthesizer()
    
    private var cancellables = Set<AnyCancellable>()
    
    init() {
        setupSpeechRecognition()
        addWelcomeMessage()
    }
    
    private func setupSpeechRecognition() {
        speechRecognizer = SFSpeechRecognizer(locale: Locale(identifier: "en-US"))
        
        SFSpeechRecognizer.requestAuthorization { authStatus in
            DispatchQueue.main.async {
                self.voiceEnabled = authStatus == .authorized
            }
        }
    }
    
    private func addWelcomeMessage() {
        let welcome = """
        # Welcome to FlowLoom Native UI
        
        I'm your AI Development Assistant with a three-pane interface:
        - **Left**: Interaction (voice or text input)
        - **Top Right**: Presentation (code, diagrams, rich content)
        - **Bottom Right**: Conversation history
        
        Try saying "Hey FlowLoom" or press the microphone to start!
        """
        
        presentationContent = welcome
        presentationFormat = .markdown
        
        conversationHistory.append(FlowLoomMessage(
            content: "FlowLoom Native UI initialized. Ready to assist!",
            type: .system,
            format: .plain
        ))
    }
    
    func sendMessage() {
        guard !userInput.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty else { return }
        
        let message = userInput
        conversationHistory.append(FlowLoomMessage(
            content: message,
            type: .user,
            format: .plain
        ))
        
        userInput = ""
        processUserInput(message)
    }
    
    func toggleListening() {
        if isListening {
            stopListening()
        } else {
            startListening()
        }
    }
    
    private func startListening() {
        guard voiceEnabled else { return }
        
        isListening = true
        
        recognitionRequest = SFSpeechAudioBufferRecognitionRequest()
        guard let recognitionRequest = recognitionRequest else { return }
        
        recognitionRequest.shouldReportPartialResults = true
        
        let inputNode = audioEngine.inputNode
        let recordingFormat = inputNode.outputFormat(forBus: 0)
        
        inputNode.installTap(onBus: 0, bufferSize: 1024, format: recordingFormat) { buffer, _ in
            self.recognitionRequest?.append(buffer)
            
            // Calculate audio level for visualization
            let channelData = buffer.floatChannelData?[0]
            let frames = buffer.frameLength
            
            var sum: Float = 0
            for i in 0..<Int(frames) {
                sum += abs(channelData?[i] ?? 0)
            }
            
            DispatchQueue.main.async {
                self.audioLevel = sum / Float(frames) * 50
            }
        }
        
        audioEngine.prepare()
        try? audioEngine.start()
        
        recognitionTask = speechRecognizer?.recognitionTask(with: recognitionRequest) { result, error in
            if let result = result {
                let transcription = result.bestTranscription.formattedString
                
                DispatchQueue.main.async {
                    self.userInput = transcription
                    
                    // Check for wake word or command completion
                    if transcription.lowercased().contains("hey flowloom") ||
                       result.isFinal {
                        self.stopListening()
                        self.sendMessage()
                    }
                }
            }
            
            if error != nil {
                self.stopListening()
            }
        }
    }
    
    private func stopListening() {
        isListening = false
        audioEngine.stop()
        audioEngine.inputNode.removeTap(onBus: 0)
        recognitionRequest?.endAudio()
        recognitionRequest = nil
        recognitionTask?.cancel()
        recognitionTask = nil
        audioLevel = 0
    }
    
    private func processUserInput(_ input: String) {
        isProcessing = true
        
        // Simulate FlowLoom processing
        Task {
            let response = await simulateFlowLoomResponse(for: input)
            
            await MainActor.run {
                // Update presentation pane if needed
                if let presentation = response.presentation {
                    self.presentationContent = presentation
                    self.presentationFormat = response.format
                }
                
                // Add to conversation
                if let conversation = response.conversation {
                    self.conversationHistory.append(FlowLoomMessage(
                        content: conversation,
                        type: .assistant,
                        format: .plain
                    ))
                    
                    // Speak response if voice is enabled
                    if self.voiceEnabled {
                        self.speak(conversation)
                    }
                }
                
                self.isProcessing = false
            }
        }
    }
    
    private func simulateFlowLoomResponse(for input: String) async -> FlowLoomResponse {
        // Simulate network delay
        try? await Task.sleep(nanoseconds: 500_000_000)
        
        let lowercased = input.lowercased()
        
        // Route different types of requests appropriately
        if lowercased.contains("create") && lowercased.contains("plan") {
            return FlowLoomResponse(
                presentation: """
                # Project Plan: \(input)
                
                ## Overview
                Generated plan based on your request.
                
                ## Tasks
                1. Research and analysis
                2. Architecture design
                3. Implementation
                4. Testing
                5. Documentation
                
                ## Timeline
                - Week 1-2: Research
                - Week 3-4: Design
                - Week 5-8: Implementation
                
                ```mermaid
                gantt
                    title Project Timeline
                    Research :a1, 2024-01-01, 2w
                    Design :a2, after a1, 2w
                    Implementation :a3, after a2, 4w
                ```
                """,
                conversation: "I've created a project plan and displayed it in the presentation pane. The plan includes tasks and timeline.",
                format: .markdown
            )
        } else if lowercased.contains("code") || lowercased.contains("function") {
            return FlowLoomResponse(
                presentation: """
                ```swift
                // FlowLoom-generated Swift function
                func processUserRequest(_ request: String) -> FlowLoomResponse {
                    // Parse the request
                    let components = request.components(separatedBy: " ")
                    
                    // Process based on intent
                    switch components.first?.lowercased() {
                    case "create":
                        return handleCreateCommand(components)
                    case "analyze":
                        return handleAnalyzeCommand(components)
                    default:
                        return FlowLoomResponse(
                            presentation: nil,
                            conversation: "Command processed",
                            format: .plain
                        )
                    }
                }
                
                private func handleCreateCommand(_ components: [String]) -> FlowLoomResponse {
                    // Implementation here
                    return FlowLoomResponse(
                        presentation: "Created successfully",
                        conversation: "Done",
                        format: .plain
                    )
                }
                ```
                """,
                conversation: "I've generated a Swift function example and displayed it with syntax highlighting.",
                format: .code(language: "swift")
            )
        } else {
            // Regular conversation
            return FlowLoomResponse(
                presentation: nil,
                conversation: "I understand you want to: \(input). Let me help you with that. In a full implementation, I would process this through FlowLoom's command system.",
                format: .plain
            )
        }
    }
    
    private func speak(_ text: String) {
        let utterance = AVSpeechUtterance(string: text)
        utterance.rate = 0.5
        utterance.voice = AVSpeechSynthesisVoice(language: "en-US")
        speechSynthesizer.speak(utterance)
    }
}

// MARK: - Views

struct FlowLoomUIView: View {
    @StateObject private var viewModel = FlowLoomViewModel()
    
    var body: some View {
        GeometryReader { geometry in
            HStack(spacing: 0) {
                // Interaction Pane (1/3 width)
                InteractionPane(viewModel: viewModel)
                    .frame(width: geometry.size.width / 3)
                
                Divider()
                
                // Presentation and Conversation Panes (2/3 width)
                VStack(spacing: 0) {
                    // Presentation Pane (2/3 height)
                    PresentationPane(viewModel: viewModel)
                        .frame(height: geometry.size.height * 2 / 3)
                    
                    Divider()
                    
                    // Conversation Pane (1/3 height)
                    ConversationPane(viewModel: viewModel)
                }
                .frame(width: geometry.size.width * 2 / 3)
            }
        }
        .frame(minWidth: 1200, minHeight: 800)
    }
}

struct InteractionPane: View {
    @ObservedObject var viewModel: FlowLoomViewModel
    @FocusState private var inputFocused: Bool
    
    var body: some View {
        VStack(spacing: 0) {
            // Header
            HStack {
                Image(systemName: "mic.fill")
                    .foregroundColor(viewModel.isListening ? .red : .secondary)
                Text("Interaction")
                    .font(.headline)
                Spacer()
            }
            .padding()
            
            Divider()
            
            // Voice visualization
            if viewModel.isListening {
                AudioVisualizerView(level: viewModel.audioLevel)
                    .frame(height: 100)
                    .padding()
            }
            
            Spacer()
            
            // Input area
            VStack(spacing: 12) {
                TextEditor(text: $viewModel.userInput)
                    .font(.system(size: 14))
                    .scrollContentBackground(.hidden)
                    .background(Color(NSColor.controlBackgroundColor))
                    .cornerRadius(8)
                    .focused($inputFocused)
                    .onSubmit {
                        viewModel.sendMessage()
                    }
                
                HStack(spacing: 12) {
                    // Voice button
                    Button(action: viewModel.toggleListening) {
                        Image(systemName: viewModel.isListening ? "stop.circle.fill" : "mic.circle.fill")
                            .font(.title)
                            .foregroundColor(viewModel.isListening ? .red : .accentColor)
                    }
                    .buttonStyle(.plain)
                    .disabled(!viewModel.voiceEnabled)
                    
                    Spacer()
                    
                    // Send button
                    Button("Send") {
                        viewModel.sendMessage()
                    }
                    .disabled(viewModel.userInput.isEmpty || viewModel.isProcessing)
                }
            }
            .padding()
            
            // Status
            if viewModel.isProcessing {
                HStack {
                    ProgressView()
                        .scaleEffect(0.8)
                    Text("Processing...")
                        .font(.caption)
                }
                .padding(.bottom)
            }
        }
        .background(Color(NSColor.windowBackgroundColor))
    }
}

struct PresentationPane: View {
    @ObservedObject var viewModel: FlowLoomViewModel
    
    var body: some View {
        VStack(spacing: 0) {
            // Header
            HStack {
                Image(systemName: "doc.richtext")
                Text("Presentation")
                    .font(.headline)
                Spacer()
                Text(formatLabel(for: viewModel.presentationFormat))
                    .font(.caption)
                    .foregroundColor(.secondary)
            }
            .padding()
            
            Divider()
            
            // Content
            RichContentView(
                content: viewModel.presentationContent,
                format: viewModel.presentationFormat
            )
        }
        .background(Color(NSColor.controlBackgroundColor))
    }
    
    func formatLabel(for format: FlowLoomMessage.ContentFormat) -> String {
        switch format {
        case .plain: return "Plain Text"
        case .markdown: return "Markdown"
        case .code(let lang): return "Code (\(lang))"
        case .diagram: return "Diagram"
        }
    }
}

struct ConversationPane: View {
    @ObservedObject var viewModel: FlowLoomViewModel
    
    var body: some View {
        VStack(spacing: 0) {
            // Header
            HStack {
                Image(systemName: "bubble.left.and.bubble.right")
                Text("Conversation")
                    .font(.headline)
                Spacer()
                Text("\(viewModel.conversationHistory.count) messages")
                    .font(.caption)
                    .foregroundColor(.secondary)
            }
            .padding()
            
            Divider()
            
            // Messages
            ScrollViewReader { proxy in
                ScrollView {
                    LazyVStack(alignment: .leading, spacing: 8) {
                        ForEach(viewModel.conversationHistory) { message in
                            ConversationMessageView(message: message)
                                .id(message.id)
                        }
                    }
                    .padding()
                }
                .onChange(of: viewModel.conversationHistory.count) { _ in
                    withAnimation {
                        proxy.scrollTo(viewModel.conversationHistory.last?.id, anchor: .bottom)
                    }
                }
            }
        }
        .background(Color(NSColor.windowBackgroundColor))
    }
}

// MARK: - Component Views

struct AudioVisualizerView: View {
    let level: Float
    
    var body: some View {
        GeometryReader { geometry in
            ZStack {
                // Background
                RoundedRectangle(cornerRadius: 8)
                    .fill(Color.black.opacity(0.05))
                
                // Waveform visualization
                HStack(spacing: 2) {
                    ForEach(0..<20) { i in
                        RoundedRectangle(cornerRadius: 2)
                            .fill(Color.accentColor)
                            .frame(width: geometry.size.width / 25)
                            .scaleEffect(y: CGFloat.random(in: 0.3...1.0) * CGFloat(level), anchor: .center)
                            .animation(.easeInOut(duration: 0.1), value: level)
                    }
                }
                .padding(8)
            }
        }
    }
}

struct ConversationMessageView: View {
    let message: FlowLoomMessage
    
    var body: some View {
        HStack(alignment: .top, spacing: 8) {
            // Icon
            Image(systemName: iconName)
                .font(.caption)
                .foregroundColor(iconColor)
            
            VStack(alignment: .leading, spacing: 2) {
                // Header
                HStack {
                    Text(roleName)
                        .font(.caption)
                        .fontWeight(.semibold)
                    Text(message.timestamp, style: .time)
                        .font(.caption)
                        .foregroundColor(.secondary)
                }
                
                // Content
                Text(message.content)
                    .font(.system(size: 13))
                    .textSelection(.enabled)
            }
            
            Spacer()
        }
        .padding(.vertical, 4)
    }
    
    var iconName: String {
        switch message.type {
        case .user: return "person.circle"
        case .assistant: return "cpu"
        case .system: return "gear"
        }
    }
    
    var iconColor: Color {
        switch message.type {
        case .user: return .blue
        case .assistant: return .green
        case .system: return .orange
        }
    }
    
    var roleName: String {
        switch message.type {
        case .user: return "You"
        case .assistant: return "FlowLoom"
        case .system: return "System"
        }
    }
}

// MARK: - Rich Content View

struct RichContentView: NSViewRepresentable {
    let content: String
    let format: FlowLoomMessage.ContentFormat
    
    func makeNSView(context: Context) -> WKWebView {
        let webView = WKWebView()
        webView.navigationDelegate = context.coordinator
        return webView
    }
    
    func updateNSView(_ webView: WKWebView, context: Context) {
        let html = generateHTML(for: content, format: format)
        webView.loadHTMLString(html, baseURL: nil)
    }
    
    func makeCoordinator() -> Coordinator {
        Coordinator()
    }
    
    class Coordinator: NSObject, WKNavigationDelegate {
        func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction, decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
            if navigationAction.navigationType == .linkActivated {
                if let url = navigationAction.request.url {
                    NSWorkspace.shared.open(url)
                    decisionHandler(.cancel)
                    return
                }
            }
            decisionHandler(.allow)
        }
    }
    
    private func generateHTML(for content: String, format: FlowLoomMessage.ContentFormat) -> String {
        let css = """
        <style>
            body {
                font-family: -apple-system, BlinkMacSystemFont, sans-serif;
                line-height: 1.6;
                color: #333;
                padding: 20px;
                background: transparent;
            }
            pre {
                background: #f4f4f4;
                border: 1px solid #ddd;
                border-radius: 4px;
                padding: 12px;
                overflow-x: auto;
            }
            code {
                background: #f4f4f4;
                padding: 2px 4px;
                border-radius: 3px;
                font-family: 'SF Mono', Monaco, monospace;
            }
            table {
                border-collapse: collapse;
                width: 100%;
                margin: 16px 0;
            }
            th, td {
                border: 1px solid #ddd;
                padding: 8px;
                text-align: left;
            }
            th {
                background: #f0f0f0;
            }
            h1, h2, h3 {
                margin-top: 24px;
                margin-bottom: 16px;
            }
            blockquote {
                border-left: 4px solid #ddd;
                margin: 0;
                padding-left: 16px;
                color: #666;
            }
            @media (prefers-color-scheme: dark) {
                body { color: #e0e0e0; }
                pre, code { background: #2a2a2a; border-color: #444; }
                table, th, td { border-color: #444; }
                th { background: #333; }
                blockquote { border-color: #666; color: #aaa; }
            }
        </style>
        """
        
        let processedContent: String
        switch format {
        case .plain:
            processedContent = content.replacingOccurrences(of: "\n", with: "<br>")
        case .markdown:
            // In real implementation, use a markdown parser
            processedContent = content
                .replacingOccurrences(of: "# ", with: "<h1>")
                .replacingOccurrences(of: "\n", with: "</h1>\n")
                .replacingOccurrences(of: "## ", with: "<h2>")
                .replacingOccurrences(of: "```", with: "<pre><code>")
                .replacingOccurrences(of: "```", with: "</code></pre>")
        case .code(let language):
            processedContent = "<pre><code class='\(language)'>\(content)</code></pre>"
        case .diagram:
            // Would integrate with mermaid.js or similar
            processedContent = "<div class='diagram'>\(content)</div>"
        }
        
        return """
        <!DOCTYPE html>
        <html>
        <head>
            <meta charset="UTF-8">
            \(css)
        </head>
        <body>
            \(processedContent)
        </body>
        </html>
        """
    }
}

// MARK: - App Delegate

class AppDelegate: NSObject, NSApplicationDelegate {
    var window: NSWindow!
    
    func applicationDidFinishLaunching(_ notification: Notification) {
        let contentView = FlowLoomUIView()
        
        window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 1400, height: 900),
            styleMask: [.titled, .closable, .miniaturizable, .resizable, .fullSizeContentView],
            backing: .buffered,
            defer: false
        )
        
        window.center()
        window.title = "FlowLoom"
        window.titlebarAppearsTransparent = true
        window.contentView = NSHostingView(rootView: contentView)
        window.makeKeyAndOrderFront(nil)
    }
    
    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool {
        true
    }
}

// MARK: - Main Entry Point

let app = NSApplication.shared
let delegate = AppDelegate()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.activate(ignoringOtherApps: true)
app.run()