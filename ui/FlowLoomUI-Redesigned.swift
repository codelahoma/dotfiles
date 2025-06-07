#!/usr/bin/env swift

import SwiftUI
import AppKit
import Combine
import WebKit

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
        case plain, markdown, code(language: String), diagram, error
    }
}

struct ResponseTab: Identifiable {
    let id = UUID()
    let content: String
    let timestamp: Date
    let title: String
    var isRead: Bool = false
    var scrollOffset: CGFloat = 0
}

// MARK: - Enhanced Markdown Renderer using WebKit

struct FlowLoomMarkdownView: NSViewRepresentable {
    let content: String
    let isDarkMode: Bool
    let fontSize: Int
    
    func makeNSView(context: Context) -> WKWebView {
        let webView = WKWebView()
        webView.navigationDelegate = context.coordinator
        
        // Configure WebView for markdown display
        let preferences = WKWebpagePreferences()
        preferences.allowsContentJavaScript = true
        webView.configuration.defaultWebpagePreferences = preferences
        
        return webView
    }
    
    func updateNSView(_ webView: WKWebView, context: Context) {
        let html = generateHTML(from: content, isDarkMode: isDarkMode)
        webView.loadHTMLString(html, baseURL: nil)
    }
    
    func makeCoordinator() -> Coordinator {
        Coordinator()
    }
    
    class Coordinator: NSObject, WKNavigationDelegate {
        func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction, decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
            // Handle link clicks
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
    
    private func generateHTML(from markdown: String, isDarkMode: Bool) -> String {
        let escapedContent = markdown
            .replacingOccurrences(of: "&", with: "&amp;")
            .replacingOccurrences(of: "<", with: "&lt;")
            .replacingOccurrences(of: ">", with: "&gt;")
        
        let theme = isDarkMode ? darkThemeCSS : lightThemeCSS
        
        return """
        <!DOCTYPE html>
        <html>
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <script src="https://cdn.jsdelivr.net/npm/marked/marked.min.js"></script>
            <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/\(isDarkMode ? "github-dark" : "github").min.css">
            <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"></script>
            <style>
                \(theme)
            </style>
        </head>
        <body>
            <div id="content"></div>
            <script>
                // Configure marked options
                marked.setOptions({
                    highlight: function(code, lang) {
                        if (lang && hljs.getLanguage(lang)) {
                            return hljs.highlight(code, { language: lang }).value;
                        }
                        return hljs.highlightAuto(code).value;
                    },
                    breaks: true,
                    gfm: true
                });
                
                // Render markdown
                const markdown = `\(escapedContent)`;
                document.getElementById('content').innerHTML = marked.parse(markdown);
                
                // Apply syntax highlighting to all code blocks
                document.querySelectorAll('pre code').forEach((block) => {
                    hljs.highlightElement(block);
                });
            </script>
        </body>
        </html>
        """
    }
    
    private var lightThemeCSS: String {
        """
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif;
            line-height: 1.6;
            color: #24292e;
            background-color: #ffffff;
            padding: 20px;
            margin: 0;
            font-size: \(fontSize)px;
        }
        
        h1, h2, h3, h4, h5, h6 {
            margin-top: 24px;
            margin-bottom: 16px;
            font-weight: 600;
            line-height: 1.25;
        }
        
        h1 { font-size: 2em; border-bottom: 1px solid #eaecef; padding-bottom: 0.3em; }
        h2 { font-size: 1.5em; border-bottom: 1px solid #eaecef; padding-bottom: 0.3em; }
        h3 { font-size: 1.25em; }
        
        code {
            padding: 0.2em 0.4em;
            margin: 0;
            font-size: 85%;
            background-color: rgba(27,31,35,0.05);
            border-radius: 3px;
            font-family: SFMono-Regular, Consolas, 'Liberation Mono', Menlo, monospace;
        }
        
        pre {
            padding: 16px;
            overflow: auto;
            font-size: 85%;
            line-height: 1.45;
            background-color: #f6f8fa;
            border-radius: 6px;
        }
        
        pre code {
            display: inline;
            padding: 0;
            margin: 0;
            overflow: visible;
            line-height: inherit;
            background-color: transparent;
        }
        
        blockquote {
            padding: 0 1em;
            color: #6a737d;
            border-left: 0.25em solid #dfe2e5;
            margin: 0;
        }
        
        table {
            border-spacing: 0;
            border-collapse: collapse;
            margin-bottom: 16px;
        }
        
        table th, table td {
            padding: 6px 13px;
            border: 1px solid #dfe2e5;
        }
        
        table tr:nth-child(2n) {
            background-color: #f6f8fa;
        }
        
        a {
            color: #0366d6;
            text-decoration: none;
        }
        
        a:hover {
            text-decoration: underline;
        }
        
        ul, ol {
            padding-left: 2em;
        }
        
        li + li {
            margin-top: 0.25em;
        }
        
        img {
            max-width: 100%;
            box-sizing: content-box;
        }
        """
    }
    
    private var darkThemeCSS: String {
        """
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif;
            line-height: 1.6;
            color: #c9d1d9;
            background-color: #0d1117;
            padding: 20px;
            margin: 0;
            font-size: \(fontSize)px;
        }
        
        h1, h2, h3, h4, h5, h6 {
            margin-top: 24px;
            margin-bottom: 16px;
            font-weight: 600;
            line-height: 1.25;
            color: #f0f6fc;
        }
        
        h1 { font-size: 2em; border-bottom: 1px solid #30363d; padding-bottom: 0.3em; }
        h2 { font-size: 1.5em; border-bottom: 1px solid #30363d; padding-bottom: 0.3em; }
        h3 { font-size: 1.25em; }
        
        code {
            padding: 0.2em 0.4em;
            margin: 0;
            font-size: 85%;
            background-color: rgba(110,118,129,0.4);
            border-radius: 3px;
            font-family: SFMono-Regular, Consolas, 'Liberation Mono', Menlo, monospace;
        }
        
        pre {
            padding: 16px;
            overflow: auto;
            font-size: 85%;
            line-height: 1.45;
            background-color: #161b22;
            border-radius: 6px;
        }
        
        pre code {
            display: inline;
            padding: 0;
            margin: 0;
            overflow: visible;
            line-height: inherit;
            background-color: transparent;
        }
        
        blockquote {
            padding: 0 1em;
            color: #8b949e;
            border-left: 0.25em solid #30363d;
            margin: 0;
        }
        
        table {
            border-spacing: 0;
            border-collapse: collapse;
            margin-bottom: 16px;
        }
        
        table th, table td {
            padding: 6px 13px;
            border: 1px solid #30363d;
        }
        
        table tr:nth-child(2n) {
            background-color: #161b22;
        }
        
        a {
            color: #58a6ff;
            text-decoration: none;
        }
        
        a:hover {
            text-decoration: underline;
        }
        
        ul, ol {
            padding-left: 2em;
        }
        
        li + li {
            margin-top: 0.25em;
        }
        
        img {
            max-width: 100%;
            box-sizing: content-box;
        }
        """
    }
}

// MARK: - View Model

@MainActor
class FlowLoomViewModel: ObservableObject {
    // UI State - Dual pane markdown viewer with tabs
    @Published var responseTabs: [ResponseTab] = []
    @Published var activeTabIndex: Int = 0
    @Published var tabCounter: Int = 0
    @Published var commentaryContent = ""
    @Published var refreshTrigger = false
    @Published var presentationFormat: FlowLoomMessage.ContentFormat = .markdown
    @Published var commentaryFormat: FlowLoomMessage.ContentFormat = .markdown
    @Published var isDarkMode = false
    
    // File watchers for both panes
    private var presentationWatcher: DispatchSourceFileSystemObject?
    private var commentaryWatcher: DispatchSourceFileSystemObject?
    private let presentationFile = URL(fileURLWithPath: "./tmp/claude-response.md")
    private let commentaryFile = URL(fileURLWithPath: "./tmp/claude-commentary.md")
    
    init() {
        setupFileWatchers()
        addWelcomeContent()
        setupDarkModeDetection()
    }
    
    private func setupDarkModeDetection() {
        // Detect initial dark mode state
        isDarkMode = NSApplication.shared.effectiveAppearance.name == .darkAqua
        
        // Listen for appearance changes
        NotificationCenter.default.addObserver(
            forName: NSApplication.didChangeScreenParametersNotification,
            object: nil,
            queue: .main
        ) { _ in
            self.isDarkMode = NSApplication.shared.effectiveAppearance.name == .darkAqua
        }
    }
    
    private func addWelcomeContent() {
        let welcomeContent = """
        # 🎉 FlowLoom Tabbed Interface
    
    ## Tabbed Presentation Area
    This upper pane now features a tabbed interface to preserve all responses.
    
    ## Features
    - **📋 Tabbed Interface**: New responses create tabs - no content loss!
    - **Rich Markdown Rendering**: Full markdown support with syntax highlighting
    - **Scroll Memory**: Each tab remembers its scroll position
    - **Read Tracking**: Visual indicators for read/unread tabs
    - **Real-time Updates**: Automatically creates new tabs when content arrives
    - **Dual-Pane Design**: Separate areas for content and commentary
    
    ## How It Works
    1. New responses create tabs on the right (newest active)
    2. Your current tab stays in position while you read
    3. Click tabs to switch between responses
    4. Close tabs with the × button as you read them
    5. Each tab preserves its scroll position
    6. Live commentary appears in the lower pane
    
    Ready for enhanced FlowLoom interaction with full conversation history! 🚀
    """
        
        addResponseTab(welcomeContent)
    
        commentaryContent = """
        ## 💬 Live Commentary Stream
        
        This lower pane shows real-time commentary as FlowLoom works:
        
        - **Progress updates** during complex tasks
        - **Thinking process** explanations
        - **Status messages** and notifications
        - **Interactive feedback** during processing
        
        *Commentary will appear here as FlowLoom processes your requests...*
        """
        
        presentationFormat = .markdown
        commentaryFormat = .markdown
    }
    

    
    // MARK: - File Watchers
    
    private func setupFileWatchers() {
        setupPresentationWatcher()
        setupCommentaryWatcher()
    }
    
    private func setupPresentationWatcher() {
        // Create the file if it doesn't exist
        try? "".write(to: presentationFile, atomically: true, encoding: .utf8)
        
        let fileDescriptor = open(presentationFile.path, O_EVTONLY)
        guard fileDescriptor >= 0 else {
            print("Failed to open presentation file for watching: \(presentationFile.path)")
            return
        }
        
        presentationWatcher = DispatchSource.makeFileSystemObjectSource(
            fileDescriptor: fileDescriptor,
            eventMask: .write,
            queue: DispatchQueue.global(qos: .background)
        )
        
        presentationWatcher?.setEventHandler { [weak self] in
            self?.handlePresentationFileChange()
        }
        
        presentationWatcher?.setCancelHandler {
            close(fileDescriptor)
        }
        
        presentationWatcher?.resume()
    }
    
    private func setupCommentaryWatcher() {
        // Create the commentary file if it doesn't exist
        try? "".write(to: commentaryFile, atomically: true, encoding: .utf8)
        
        let fileDescriptor = open(commentaryFile.path, O_EVTONLY)
        guard fileDescriptor >= 0 else {
            print("Failed to open commentary file for watching: \(commentaryFile.path)")
            return
        }
        
        commentaryWatcher = DispatchSource.makeFileSystemObjectSource(
            fileDescriptor: fileDescriptor,
            eventMask: .write,
            queue: DispatchQueue.global(qos: .background)
        )
        
        commentaryWatcher?.setEventHandler { [weak self] in
            self?.handleCommentaryFileChange()
        }
        
        commentaryWatcher?.setCancelHandler {
            close(fileDescriptor)
        }
        
        commentaryWatcher?.resume()
    }
    
    private func handlePresentationFileChange() {
        DispatchQueue.main.async {
            self.loadPresentationFromFile()
        }
    }
    
    private func handleCommentaryFileChange() {
        DispatchQueue.main.async {
            self.loadCommentaryFromFile()
        }
    }
    
    private func loadPresentationFromFile() {
        do {
            let content = try String(contentsOf: presentationFile, encoding: .utf8)
            if !content.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
                // Create new tab instead of replacing content
                addResponseTab(content)
                self.presentationFormat = .markdown
                
                // Force UI refresh
                self.refreshTrigger.toggle()
                self.objectWillChange.send()
            }
        } catch {
            print("Error reading presentation file: \(error.localizedDescription)")
        }
    }
    
    private func loadCommentaryFromFile() {
        do {
            let content = try String(contentsOf: commentaryFile, encoding: .utf8)
            if !content.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
                self.commentaryContent = content
                self.commentaryFormat = .markdown
                
                // Force UI refresh
                self.refreshTrigger.toggle()
                self.objectWillChange.send()
            }
        } catch {
            print("Error reading commentary file: \(error.localizedDescription)")
        }
    }
    
    // MARK: - Tab Management
    
    func addResponseTab(_ content: String) {
        tabCounter += 1
        let tab = ResponseTab(
            content: content,
            timestamp: Date(),
            title: extractTitle(content) ?? "Response \(tabCounter)"
        )
        responseTabs.append(tab)  // Add to end (rightmost)
        activeTabIndex = responseTabs.count - 1  // Focus newest (rightmost)
    }
    
    private func extractTitle(_ content: String) -> String? {
        let lines = content.components(separatedBy: .newlines)
        for line in lines {
            let trimmed = line.trimmingCharacters(in: .whitespacesAndNewlines)
            if trimmed.hasPrefix("# ") {
                return String(trimmed.dropFirst(2)).trimmingCharacters(in: .whitespacesAndNewlines)
            }
        }
        return nil
    }
    
    func selectTab(_ index: Int) {
        guard index >= 0 && index < responseTabs.count else { return }
        activeTabIndex = index
        markTabAsRead(index)
    }
    
    func closeTab(_ tabId: UUID) {
        guard let index = responseTabs.firstIndex(where: { $0.id == tabId }) else { return }
        responseTabs.remove(at: index)
        
        // Adjust active tab index
        if responseTabs.isEmpty {
            activeTabIndex = 0
        } else if index <= activeTabIndex {
            activeTabIndex = max(0, activeTabIndex - 1)
        }
    }
    
    func markTabAsRead(_ index: Int) {
        guard index >= 0 && index < responseTabs.count else { return }
        responseTabs[index].isRead = true
    }
    
    func updateTabScrollOffset(_ index: Int, offset: CGFloat) {
        guard index >= 0 && index < responseTabs.count else { return }
        responseTabs[index].scrollOffset = offset
    }
    
    deinit {
        presentationWatcher?.cancel()
        commentaryWatcher?.cancel()
    }
}

// MARK: - Main UI View

struct FlowLoomRedesignedView: View {
    @StateObject private var viewModel = FlowLoomViewModel()
    @State private var commentaryHeight: CGFloat = 200
    
    var body: some View {
        GeometryReader { geometry in
            VStack(spacing: 0) {
                // Upper pane: Main presentation (larger)
                PresentationView(viewModel: viewModel)
                    .frame(height: geometry.size.height - commentaryHeight)
                
                // Resize handle
                Rectangle()
                    .fill(Color.gray.opacity(0.001))
                    .frame(height: 4)
                    .overlay(
                        Rectangle()
                            .fill(Color.gray.opacity(0.3))
                            .frame(height: 1)
                    )
                    .onHover { hovering in
                        if hovering {
                            NSCursor.resizeUpDown.push()
                        } else {
                            NSCursor.pop()
                        }
                    }
                    .gesture(
                        DragGesture()
                            .onChanged { value in
                                let newHeight = commentaryHeight - value.translation.height
                                commentaryHeight = max(50, min(geometry.size.height - 100, newHeight))
                            }
                    )
                
                // Lower pane: Commentary stream (smaller)
                CommentaryView(viewModel: viewModel)
                    .frame(height: commentaryHeight)
            }
        }
        .frame(minWidth: 800, minHeight: 600)
    }
}

// MARK: - Presentation View

struct PresentationView: View {
    @ObservedObject var viewModel: FlowLoomViewModel
    
    var body: some View {
        VStack(spacing: 0) {
            // Header
            HStack {
                Image(systemName: "doc.richtext")
                Text("Presentation")
                    .font(.headline)
                
                Spacer()
                
                FormatIndicator(format: viewModel.presentationFormat)
                
                Button(action: copyCurrentTabContent) {
                    Image(systemName: "doc.on.doc")
                        .font(.caption)
                }
                .buttonStyle(.plain)
                .help("Copy current tab content")
            }
            .padding(.horizontal)
            .padding(.vertical, 8)
            .background(Color(NSColor.windowBackgroundColor))
            
            // Tab Bar
            if !viewModel.responseTabs.isEmpty {
                ScrollView(.horizontal, showsIndicators: false) {
                    HStack(spacing: 4) {
                        ForEach(Array(viewModel.responseTabs.enumerated()), id: \.1.id) { index, tab in
                            TabButton(
                                tab: tab,
                                isActive: index == viewModel.activeTabIndex,
                                viewModel: viewModel
                            ) {
                                viewModel.selectTab(index)
                            }
                        }
                    }
                    .padding(.horizontal)
                }
                .frame(height: 44)
                .background(Color(NSColor.windowBackgroundColor).opacity(0.5))
            }
            
            Divider()
            
            // Active Tab Content
            if !viewModel.responseTabs.isEmpty && viewModel.activeTabIndex < viewModel.responseTabs.count {
                TabContentView(
                    tab: viewModel.responseTabs[viewModel.activeTabIndex],
                    tabIndex: viewModel.activeTabIndex,
                    viewModel: viewModel
                )
            } else {
                // Empty state
                VStack {
                    Spacer()
                    Text("No content available")
                        .foregroundColor(.secondary)
                    Spacer()
                }
            }
        }
        .background(Color(NSColor.controlBackgroundColor))
    }
    
    private func copyCurrentTabContent() {
        guard !viewModel.responseTabs.isEmpty && viewModel.activeTabIndex < viewModel.responseTabs.count else { return }
        let content = viewModel.responseTabs[viewModel.activeTabIndex].content
        NSPasteboard.general.clearContents()
        NSPasteboard.general.setString(content, forType: .string)
    }
}

// MARK: - Commentary View

struct CommentaryView: View {
    @ObservedObject var viewModel: FlowLoomViewModel
    
    var body: some View {
        VStack(spacing: 0) {
            // Header
            HStack {
                Image(systemName: "bubble.left.and.bubble.right")
                Text("Live Commentary")
                    .font(.headline)
                
                Spacer()
                
                FormatIndicator(format: viewModel.commentaryFormat)
                
                Button(action: copyCommentary) {
                    Image(systemName: "doc.on.doc")
                        .font(.caption)
                }
                .buttonStyle(.plain)
                .help("Copy commentary")
            }
            .padding(.horizontal)
            .padding(.vertical, 8)
            .background(Color(NSColor.windowBackgroundColor))
            
            Divider()
            
            // Content with enhanced markdown rendering
            FlowLoomMarkdownView(
                content: viewModel.commentaryContent,
                isDarkMode: viewModel.isDarkMode,
                fontSize: 12
            )
        }
        .background(Color(NSColor.controlBackgroundColor).opacity(0.3))
    }
    
    private func copyCommentary() {
        NSPasteboard.general.clearContents()
        NSPasteboard.general.setString(viewModel.commentaryContent, forType: .string)
    }
}

// MARK: - Component Views

struct FormatIndicator: View {
    let format: FlowLoomMessage.ContentFormat
    
    var body: some View {
        Text(formatLabel)
            .font(.caption)
            .padding(.horizontal, 8)
            .padding(.vertical, 2)
            .background(Color.accentColor.opacity(0.2))
            .cornerRadius(4)
    }
    
    var formatLabel: String {
        switch format {
        case .plain: return "Plain"
        case .markdown: return "Markdown"
        case .code(let lang): return lang.uppercased()
        case .diagram: return "Diagram"
        case .error: return "Error"
        }
    }
}

struct TabButton: View {
    let tab: ResponseTab
    let isActive: Bool
    let viewModel: FlowLoomViewModel
    let action: () -> Void
    
    var body: some View {
        HStack(spacing: 8) {
            // Unread indicator
            if !tab.isRead {
                Circle()
                    .fill(Color.accentColor)
                    .frame(width: 6, height: 6)
            }
            
            // Tab title (truncated)
            Text(tab.title)
                .font(.system(size: 13, weight: tab.isRead ? .regular : .medium))
                .lineLimit(1)
                .frame(maxWidth: 120)
            
            // Close button
            Button("×") { 
                viewModel.closeTab(tab.id) 
            }
            .font(.system(size: 16, weight: .medium))
            .foregroundColor(.secondary)
            .buttonStyle(.plain)
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 8)
        .background(isActive ? Color.accentColor : Color.clear)
        .foregroundColor(isActive ? .white : .primary)
        .cornerRadius(6)
        .onTapGesture(perform: action)
    }
}

struct TabContentView: View {
    let tab: ResponseTab
    let tabIndex: Int
    let viewModel: FlowLoomViewModel
    
    var body: some View {
        FlowLoomMarkdownView(
            content: tab.content,
            isDarkMode: viewModel.isDarkMode,
            fontSize: 14
        )
    }
}


// MARK: - App Delegate

class AppDelegate: NSObject, NSApplicationDelegate {
    var window: NSWindow!
    
    func applicationDidFinishLaunching(_ notification: Notification) {
        let contentView = FlowLoomRedesignedView()
        
        window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 1200, height: 800),
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

// MARK: - Main Entry

let app = NSApplication.shared
let delegate = AppDelegate()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.activate(ignoringOtherApps: true)
app.run()