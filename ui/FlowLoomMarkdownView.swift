#!/usr/bin/env swift

import SwiftUI
import AppKit
import WebKit

// MARK: - Enhanced Markdown Renderer using WebKit

struct FlowLoomMarkdownView: NSViewRepresentable {
    let content: String
    let isDarkMode: Bool
    
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

// MARK: - Test App

struct TestMarkdownView: View {
    @State private var isDarkMode = true
    @State private var testContent = """
    # FlowLoom Markdown Test
    
    This is a **comprehensive test** of the markdown rendering capabilities.
    
    ## Features
    
    - ✅ **Bold** and *italic* text
    - ✅ [Links](https://github.com/rodk/flowloom)
    - ✅ `inline code`
    - ✅ Lists (like this one!)
    
    ### Code Blocks
    
    ```swift
    struct FlowLoomApp {
        let name = "FlowLoom"
        let version = "1.0.0"
        
        func greet() {
            print("Hello from \\(name)!")
        }
    }
    ```
    
    ### Tables
    
    | Feature | Status | Notes |
    |---------|--------|-------|
    | Markdown | ✅ | Full support |
    | Syntax Highlighting | ✅ | Multiple languages |
    | Dark Mode | ✅ | Automatic |
    
    ### Blockquotes
    
    > FlowLoom enhances developer productivity through intelligent AI assistance.
    
    ### Task Lists
    
    - [x] Create markdown renderer
    - [x] Add syntax highlighting
    - [x] Support dark mode
    - [ ] Add mermaid diagrams
    
    ---
    
    That's it! 🎉
    """
    
    var body: some View {
        VStack(spacing: 0) {
            // Header
            HStack {
                Text("FlowLoom Markdown Test")
                    .font(.headline)
                
                Spacer()
                
                Toggle("Dark Mode", isOn: $isDarkMode)
                    .toggleStyle(.switch)
            }
            .padding()
            .background(Color(NSColor.windowBackgroundColor))
            
            Divider()
            
            // Markdown content
            FlowLoomMarkdownView(content: testContent, isDarkMode: isDarkMode)
        }
        .frame(width: 800, height: 600)
    }
}

// MARK: - App Entry Point (for testing)

class AppDelegate: NSObject, NSApplicationDelegate {
    var window: NSWindow!
    
    func applicationDidFinishLaunching(_ notification: Notification) {
        let contentView = TestMarkdownView()
        
        window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 800, height: 600),
            styleMask: [.titled, .closable, .miniaturizable, .resizable],
            backing: .buffered,
            defer: false
        )
        
        window.center()
        window.title = "FlowLoom Markdown Test"
        window.contentView = NSHostingView(rootView: contentView)
        window.makeKeyAndOrderFront(nil)
    }
    
    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool {
        true
    }
}

// Uncomment to run as standalone test app
// let app = NSApplication.shared
// let delegate = AppDelegate()
// app.delegate = delegate
// app.setActivationPolicy(.regular)
// app.activate(ignoringOtherApps: true)
// app.run()