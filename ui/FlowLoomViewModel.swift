import SwiftUI
import Combine

// MARK: - FlowLoom View Model

@MainActor
class FlowLoomViewModel: ObservableObject {
    // UI State
    @Published var userInput = ""
    @Published var conversationHistory: [FlowLoomMessage] = []
    @Published var presentationContent = ""
    @Published var presentationFormat: FlowLoomMessage.ContentFormat = .markdown
    @Published var isListening = false
    @Published var isProcessing = false
    @Published var voiceEnabled = true
    @Published var audioLevel: Float = 0.0
    
    // Three-panel state
    @Published var activityLog: [ActivityEntry] = []
    @Published var connectionStatus: ConnectionStatus = .disconnected
    @Published var memoryStatus = "32MB used"
    @Published var sessionId = UUID().uuidString
    @Published var autoTrackStatus = true
    @Published var performanceMetric = "Fast"
    @Published var lastError: String?
    
    // Command management
    @Published var commandHistory: [String] = []
    @Published var commandSuggestions: [String] = []
    
    // Markdown viewer state
    @Published var showMarkdownViewer = false
    @Published var selectedMarkdownFile: URL?
    
    // Private properties
    private var cancellables = Set<AnyCancellable>()
    private let maxCommandHistory = 100
    
    enum ConnectionStatus {
        case connected, connecting, disconnected, error(String)
    }
    
    init() {
        setupInitialState()
        loadCommandHistory()
        startMockActivityUpdates()
    }
    
    // MARK: - Setup
    
    private func setupInitialState() {
        // Add welcome message
        logActivity(.system, "FlowLoom UI initialized")
        logActivity(.info, "Ready for input")
        
        // Load saved preferences
        if let savedHistory = UserDefaults.standard.array(forKey: "flowloom.commandHistory") as? [String] {
            commandHistory = savedHistory
        }
    }
    
    private func loadCommandHistory() {
        // Load from UserDefaults or persistent storage
        // For now, add some example commands
        commandSuggestions = [
            "/help",
            "/project:plan:create",
            "/code:generate",
            "/memory:search",
            "/test:run"
        ]
    }
    
    // MARK: - Activity Management
    
    func logActivity(_ type: ActivityEntry.ActivityType, _ message: String) {
        let entry = ActivityEntry(timestamp: Date(), type: type, message: message)
        
        activityLog.append(entry)
        
        // Keep only last 1000 entries
        if activityLog.count > 1000 {
            activityLog.removeFirst()
        }
    }
    
    // MARK: - Command Processing
    
    func processInput(_ input: String) {
        let trimmedInput = input.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmedInput.isEmpty else { return }
        
        // Log the command
        logActivity(.command, trimmedInput)
        
        // Add to history
        commandHistory.insert(trimmedInput, at: 0)
        if commandHistory.count > maxCommandHistory {
            commandHistory.removeLast()
        }
        
        // Save history
        UserDefaults.standard.set(commandHistory, forKey: "flowloom.commandHistory")
        
        // Process the command
        isProcessing = true
        
        // Simulate processing
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) { [weak self] in
            self?.isProcessing = false
            self?.handleCommand(trimmedInput)
        }
    }
    
    private func handleCommand(_ command: String) {
        // Simple command handling for demo
        if command.hasPrefix("/") {
            logActivity(.system, "Processing command: \(command)")
            
            // Simulate response
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) { [weak self] in
                self?.logActivity(.response, "Command executed successfully")
            }
        } else {
            // Regular message
            let message = FlowLoomMessage(
                content: command,
                type: .user,
                format: .plain
            )
            conversationHistory.append(message)
            
            // Simulate response
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) { [weak self] in
                let response = FlowLoomMessage(
                    content: "I received your message: \"\(command)\"",
                    type: .assistant,
                    format: .plain
                )
                self?.conversationHistory.append(response)
                self?.logActivity(.response, "Generated response")
            }
        }
    }
    
    func updateCommandSuggestions() {
        // Update suggestions based on current input
        let input = userInput.lowercased()
        
        if input.isEmpty {
            commandSuggestions = ["/help", "/project:plan:create", "/code:generate"]
        } else if input.hasPrefix("/") {
            commandSuggestions = [
                "/help",
                "/project:plan:create",
                "/project:git:sync",
                "/code:generate",
                "/memory:search",
                "/test:run",
                "/session:start",
                "/mama:status"
            ].filter { $0.lowercased().contains(input) }
        } else {
            commandSuggestions = []
        }
    }
    
    // MARK: - System Status Updates
    
    func updateConnectionStatus(_ status: ConnectionStatus) {
        connectionStatus = status
        
        switch status {
        case .connected:
            logActivity(.system, "Connected to FlowLoom bridge")
        case .connecting:
            logActivity(.system, "Connecting to FlowLoom bridge...")
        case .disconnected:
            logActivity(.system, "Disconnected from FlowLoom bridge")
        case .error(let msg):
            logActivity(.error, "Connection error: \(msg)")
            lastError = msg
        }
    }
    
    func updateMemoryStatus() {
        // Simulate memory updates
        let used = Int.random(in: 20...100)
        memoryStatus = "\(used)MB used"
    }
    
    func updatePerformanceMetric() {
        // Simulate performance updates
        let metrics = ["Fast", "Normal", "Slow", "Optimizing..."]
        performanceMetric = metrics.randomElement() ?? "Normal"
    }
    
    // MARK: - Markdown File Handling
    
    func openMarkdownFile(_ url: URL) {
        selectedMarkdownFile = url
        showMarkdownViewer = true
        logActivity(.info, "Opened markdown file: \(url.lastPathComponent)")
    }
    
    // MARK: - Mock Updates (for demo)
    
    private func startMockActivityUpdates() {
        // Simulate periodic system updates
        Timer.publish(every: 5, on: .main, in: .common)
            .autoconnect()
            .sink { [weak self] _ in
                self?.updateMemoryStatus()
                self?.updatePerformanceMetric()
                
                // Random activity
                if Bool.random() {
                    let activities = [
                        "Background sync completed",
                        "Auto-save triggered",
                        "Memory snapshot created",
                        "Health check passed",
                        "Cache cleared"
                    ]
                    if let activity = activities.randomElement() {
                        self?.logActivity(.system, activity)
                    }
                }
            }
            .store(in: &cancellables)
    }
}

// MARK: - FlowLoom Message Model

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