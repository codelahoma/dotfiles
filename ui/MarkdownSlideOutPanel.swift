import SwiftUI
import WebKit

// MARK: - Markdown Slide-out Panel

struct MarkdownSlideOutPanel: View {
    @Binding var isShowing: Bool
    @Binding var markdownFile: URL?
    @State private var markdownContent: String = ""
    @State private var isLoading = false
    @State private var error: String?
    @State private var slideOffset: CGFloat = 600
    @State private var isDarkMode = false
    @State private var fontSize: CGFloat = 14
    
    var body: some View {
        ZStack(alignment: .trailing) {
            // Dimmed backdrop
            if isShowing {
                Color.black.opacity(0.3)
                    .ignoresSafeArea()
                    .onTapGesture {
                        withAnimation(.spring(response: 0.4, dampingFraction: 0.8)) {
                            isShowing = false
                        }
                    }
                    .transition(.opacity)
            }
            
            // Slide-out panel
            HStack(spacing: 0) {
                Spacer()
                
                if isShowing {
                    VStack(spacing: 0) {
                        // Header
                        panelHeader
                        
                        Divider()
                        
                        // Content area
                        ZStack {
                            if isLoading {
                                ProgressView("Loading markdown...")
                                    .frame(maxWidth: .infinity, maxHeight: .infinity)
                            } else if let error = error {
                                errorView(error)
                            } else {
                                markdownContentView
                            }
                        }
                        .frame(maxWidth: .infinity, maxHeight: .infinity)
                    }
                    .frame(width: slideOffset)
                    .background(Color(NSColor.windowBackgroundColor))
                    .shadow(color: .black.opacity(0.2), radius: 20, x: -5, y: 0)
                    .transition(.asymmetric(
                        insertion: .move(edge: .trailing).combined(with: .opacity),
                        removal: .move(edge: .trailing).combined(with: .opacity)
                    ))
                }
            }
        }
        .onChange(of: markdownFile) { newFile in
            if let file = newFile, isShowing {
                loadMarkdownFile(file)
            }
        }
        .onChange(of: isShowing) { showing in
            if showing, let file = markdownFile {
                loadMarkdownFile(file)
            }
        }
        .onAppear {
            isDarkMode = NSApp.effectiveAppearance.name == .darkAqua
        }
    }
    
    // MARK: - Header View
    
    private var panelHeader: some View {
        HStack(spacing: 12) {
            Image(systemName: "doc.text")
                .font(.title3)
                .foregroundColor(.secondary)
            
            VStack(alignment: .leading, spacing: 2) {
                Text(markdownFile?.lastPathComponent ?? "Markdown Viewer")
                    .font(.headline)
                    .lineLimit(1)
                
                if let file = markdownFile {
                    Text(file.deletingLastPathComponent().path)
                        .font(.caption)
                        .foregroundColor(.secondary)
                        .lineLimit(1)
                }
            }
            
            Spacer()
            
            // Font size controls
            HStack(spacing: 4) {
                Button(action: { fontSize = max(10, fontSize - 2) }) {
                    Image(systemName: "textformat.size.smaller")
                }
                .buttonStyle(.plain)
                
                Text("\(Int(fontSize))pt")
                    .font(.caption)
                    .foregroundColor(.secondary)
                    .frame(width: 35)
                
                Button(action: { fontSize = min(24, fontSize + 2) }) {
                    Image(systemName: "textformat.size.larger")
                }
                .buttonStyle(.plain)
            }
            
            Divider()
                .frame(height: 20)
            
            // Action buttons
            Button(action: { reloadContent() }) {
                Image(systemName: "arrow.clockwise")
            }
            .buttonStyle(.plain)
            .help("Reload")
            
            Button(action: { copyMarkdownPath() }) {
                Image(systemName: "doc.on.doc")
            }
            .buttonStyle(.plain)
            .help("Copy path")
            
            Button(action: { openInExternalEditor() }) {
                Image(systemName: "arrow.up.forward.square")
            }
            .buttonStyle(.plain)
            .help("Open in external editor")
            
            Button(action: {
                withAnimation(.spring(response: 0.4, dampingFraction: 0.8)) {
                    isShowing = false
                }
            }) {
                Image(systemName: "xmark.circle.fill")
                    .font(.title2)
                    .foregroundColor(.secondary)
            }
            .buttonStyle(.plain)
        }
        .padding(.horizontal, 16)
        .padding(.vertical, 12)
    }
    
    // MARK: - Content Views
    
    private var markdownContentView: some View {
        VStack(spacing: 0) {
            // Toolbar
            HStack {
                Text("\(markdownContent.count) characters • \(lineCount) lines")
                    .font(.caption)
                    .foregroundColor(.secondary)
                
                Spacer()
                
                Toggle(isOn: $isDarkMode) {
                    Label("Dark Mode", systemImage: isDarkMode ? "moon.fill" : "sun.max")
                }
                .toggleStyle(.button)
                .controlSize(.small)
            }
            .padding(.horizontal, 16)
            .padding(.vertical, 8)
            .background(Color(NSColor.controlBackgroundColor))
            
            // Markdown viewer
            DedicatedMarkdownView(
                content: markdownContent,
                isDarkMode: isDarkMode,
                fontSize: fontSize
            )
        }
    }
    
    private func errorView(_ error: String) -> some View {
        VStack(spacing: 12) {
            Image(systemName: "exclamationmark.triangle")
                .font(.largeTitle)
                .foregroundColor(.red)
            
            Text("Failed to load markdown")
                .font(.headline)
            
            Text(error)
                .font(.caption)
                .foregroundColor(.secondary)
                .multilineTextAlignment(.center)
                .padding(.horizontal)
            
            Button("Try Again") {
                if let file = markdownFile {
                    loadMarkdownFile(file)
                }
            }
            .buttonStyle(.borderedProminent)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }
    
    // MARK: - Helper Properties
    
    private var lineCount: Int {
        markdownContent.components(separatedBy: .newlines).count
    }
    
    // MARK: - Actions
    
    private func loadMarkdownFile(_ url: URL) {
        isLoading = true
        error = nil
        
        DispatchQueue.global(qos: .userInitiated).async {
            do {
                let content = try String(contentsOf: url, encoding: .utf8)
                DispatchQueue.main.async {
                    self.markdownContent = content
                    self.isLoading = false
                }
            } catch {
                DispatchQueue.main.async {
                    self.error = error.localizedDescription
                    self.isLoading = false
                }
            }
        }
    }
    
    private func reloadContent() {
        if let file = markdownFile {
            loadMarkdownFile(file)
        }
    }
    
    private func copyMarkdownPath() {
        if let file = markdownFile {
            NSPasteboard.general.clearContents()
            NSPasteboard.general.setString(file.path, forType: .string)
        }
    }
    
    private func openInExternalEditor() {
        if let file = markdownFile {
            NSWorkspace.shared.open(file)
        }
    }
}

// MARK: - Dedicated Markdown View

struct DedicatedMarkdownView: NSViewRepresentable {
    let content: String
    let isDarkMode: Bool
    let fontSize: CGFloat
    
    func makeNSView(context: Context) -> WKWebView {
        let config = WKWebViewConfiguration()
        config.preferences.javaScriptEnabled = true
        
        let webView = WKWebView(frame: .zero, configuration: config)
        webView.navigationDelegate = context.coordinator
        webView.setValue(false, forKey: "drawsBackground")
        
        return webView
    }
    
    func updateNSView(_ webView: WKWebView, context: Context) {
        let html = generateHTML()
        webView.loadHTMLString(html, baseURL: nil)
    }
    
    func makeCoordinator() -> Coordinator {
        Coordinator()
    }
    
    private func generateHTML() -> String {
        let escapedContent = content
            .replacingOccurrences(of: "\\", with: "\\\\")
            .replacingOccurrences(of: "`", with: "\\`")
            .replacingOccurrences(of: "$", with: "\\$")
        
        return """
        <!DOCTYPE html>
        <html>
        <head>
            <meta charset="utf-8">
            <meta name="viewport" content="width=device-width, initial-scale=1">
            <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.2.0/github-markdown-\(isDarkMode ? "dark" : "light").min.css">
            <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/themes/prism-\(isDarkMode ? "tomorrow" : "default").min.css">
            <style>
                body {
                    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif;
                    font-size: \(fontSize)px;
                    line-height: 1.6;
                    background: \(isDarkMode ? "#1e1e1e" : "#ffffff");
                    margin: 0;
                    padding: 0;
                }
                .markdown-body {
                    box-sizing: border-box;
                    min-width: 200px;
                    max-width: 980px;
                    margin: 0 auto;
                    padding: 45px;
                }
                @media (max-width: 767px) {
                    .markdown-body {
                        padding: 15px;
                    }
                }
                pre {
                    background-color: \(isDarkMode ? "#2d2d2d" : "#f6f8fa") !important;
                }
                code {
                    background-color: \(isDarkMode ? "#2d2d2d" : "#f3f4f6") !important;
                    padding: 0.2em 0.4em;
                    border-radius: 3px;
                }
                /* Scrollbar styling */
                ::-webkit-scrollbar {
                    width: 8px;
                    height: 8px;
                }
                ::-webkit-scrollbar-track {
                    background: \(isDarkMode ? "#2d2d2d" : "#f1f1f1");
                }
                ::-webkit-scrollbar-thumb {
                    background: \(isDarkMode ? "#555" : "#888");
                    border-radius: 4px;
                }
                ::-webkit-scrollbar-thumb:hover {
                    background: \(isDarkMode ? "#777" : "#555");
                }
            </style>
            <script src="https://cdn.jsdelivr.net/npm/marked/marked.min.js"></script>
            <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/prism.min.js"></script>
            <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/prism-swift.min.js"></script>
            <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/prism-python.min.js"></script>
            <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/prism-javascript.min.js"></script>
            <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/prism-bash.min.js"></script>
            <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/prism-json.min.js"></script>
        </head>
        <body>
            <div class="markdown-body" id="content"></div>
            <script>
                // Configure marked
                marked.setOptions({
                    highlight: function(code, lang) {
                        if (Prism.languages[lang]) {
                            return Prism.highlight(code, Prism.languages[lang], lang);
                        }
                        return code;
                    },
                    breaks: true,
                    gfm: true
                });
                
                // Render markdown
                const content = `\(escapedContent)`;
                document.getElementById('content').innerHTML = marked.parse(content);
                
                // Apply syntax highlighting to any unmarked code blocks
                Prism.highlightAll();
            </script>
        </body>
        </html>
        """
    }
    
    class Coordinator: NSObject, WKNavigationDelegate {
        func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction,
                     decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
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
}