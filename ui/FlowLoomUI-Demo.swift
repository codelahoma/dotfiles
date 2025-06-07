#!/usr/bin/env swift

import Foundation

// MARK: - FlowLoom UI Demo Script
// Demonstrates the capabilities of FlowLoom Native UI

struct FlowLoomUIDemo {
    static func main() {
        print("""
        🎭 FlowLoom Native UI Demo
        ==========================
        
        This demo will showcase the key features of FlowLoom's native macOS interface.
        
        """)
        
        // Check if UI exists
        guard checkUIAvailability() else {
            print("❌ FlowLoom UI not found. Please ensure FlowLoomUI-Enhanced.swift is in the same directory.")
            exit(1)
        }
        
        showMenu()
    }
    
    static func checkUIAvailability() -> Bool {
        let uiPaths = [
            "./FlowLoomUI-Enhanced.swift",
            "./FlowLoomUI.swift"
        ]
        
        for path in uiPaths {
            if FileManager.default.fileExists(atPath: path) {
                return true
            }
        }
        
        return false
    }
    
    static func showMenu() {
        while true {
            print("""
            
            Choose a demo:
            
            1. 🎤 Voice Interaction Demo
            2. 📝 Command Execution Demo
            3. 🎨 Rich Content Rendering Demo
            4. 🔄 Multi-Agent Coordination Demo
            5. 🚀 Launch Full UI
            6. 📋 Show Sample Commands
            7. 🔧 Test Installation
            
            0. Exit
            
            Enter your choice:
            """, terminator: " ")
            
            guard let input = readLine(), let choice = Int(input) else {
                print("Invalid input. Please enter a number.")
                continue
            }
            
            switch choice {
            case 0:
                print("👋 Goodbye!")
                exit(0)
            case 1:
                demoVoiceInteraction()
            case 2:
                demoCommandExecution()
            case 3:
                demoRichContent()
            case 4:
                demoMultiAgent()
            case 5:
                launchFullUI()
            case 6:
                showSampleCommands()
            case 7:
                testInstallation()
            default:
                print("Invalid choice. Please try again.")
            }
        }
    }
    
    static func demoVoiceInteraction() {
        print("""
        
        🎤 Voice Interaction Demo
        =========================
        
        FlowLoom UI supports sophisticated voice interaction:
        
        Features demonstrated:
        ✓ Wake word activation: "Hey FlowLoom"
        ✓ Continuous speech recognition
        ✓ Real-time transcription display
        ✓ Natural language processing
        ✓ Text-to-speech responses
        
        Example voice commands:
        - "Hey FlowLoom, create a project plan for a todo app"
        - "Hey FlowLoom, show me how to parse JSON in Swift"
        - "Hey FlowLoom, explain recursive self-improvement"
        
        The UI will:
        1. Show animated waveform during speech
        2. Display transcription in real-time
        3. Process command when you pause
        4. Speak the response back to you
        5. Display rich content in presentation pane
        
        Press Enter to continue...
        """)
        _ = readLine()
    }
    
    static func demoCommandExecution() {
        print("""
        
        📝 Command Execution Demo
        =========================
        
        FlowLoom executes commands through multiple interfaces:
        
        1. Command Palette (⌘K):
           - Fuzzy search through all commands
           - Shows command syntax
           - Keyboard navigation
           - Recent command history
        
        2. Direct Input:
           - Type commands directly
           - Auto-completion suggestions
           - Syntax highlighting
           - Parameter hints
        
        3. Natural Language:
           - "Create a new plan"
           - "Generate Swift code"
           - "Review my project"
           - "Run the tests"
        
        Example command flow:
        plan:create "Mobile App" → Creates structured plan
                                → Displays in presentation pane
                                → Adds to conversation history
                                → Saves to FlowLoom memory
        
        Press Enter to continue...
        """)
        _ = readLine()
    }
    
    static func demoRichContent() {
        print("""
        
        🎨 Rich Content Rendering Demo
        ==============================
        
        The presentation pane supports multiple content formats:
        
        1. Markdown Rendering:
           - Headers, lists, tables
           - Code blocks with syntax highlighting
           - Links and images
           - Block quotes and emphasis
        
        2. Code Display:
           - 150+ language support
           - Line numbers
           - Copy button
           - Theme selection
        
        3. Diagrams:
           ```mermaid
           graph TD
               A[User Input] --> B[FlowLoom]
               B --> C[Presentation]
               B --> D[Conversation]
           ```
        
        4. Tables:
           | Feature | Status | Notes |
           |---------|--------|-------|
           | Voice   | ✅     | Ready |
           | UI      | ✅     | Ready |
           | Bridge  | ✅     | Ready |
        
        5. Interactive Elements:
           - Clickable links
           - Expandable sections
           - Zoom controls
           - Export options
        
        Press Enter to continue...
        """)
        _ = readLine()
    }
    
    static func demoMultiAgent() {
        print("""
        
        🔄 Multi-Agent Coordination Demo
        ================================
        
        FlowLoom can coordinate multiple Claude instances:
        
        Scenario: Building a Web Application
        
        1. Architecture Agent:
           - Designs system components
           - Creates technical specifications
           - Reviews implementation
        
        2. Frontend Agent:
           - Implements UI components
           - Handles user interactions
           - Manages state
        
        3. Backend Agent:
           - Builds API endpoints
           - Manages database
           - Handles authentication
        
        4. Testing Agent:
           - Writes test cases
           - Performs integration testing
           - Validates functionality
        
        The UI shows:
        - Active agent indicators
        - Task assignment status
        - Progress visualization
        - Inter-agent communication
        - Unified output display
        
        All coordinated through FlowLoom's memory graph!
        
        Press Enter to continue...
        """)
        _ = readLine()
    }
    
    static func launchFullUI() {
        print("""
        
        🚀 Launching FlowLoom UI...
        
        The UI will open with:
        - Three-pane layout
        - Voice input ready
        - Demo mode active
        
        Try these actions:
        1. Click the microphone and say "Hey FlowLoom"
        2. Press ⌘K to open command palette
        3. Type a natural language request
        4. Drag pane dividers to resize
        5. Search conversation history
        
        """)
        
        // Launch the UI
        let task = Process()
        task.executableURL = URL(fileURLWithPath: "/usr/bin/swift")
        task.arguments = ["./FlowLoomUI-Enhanced.swift"]
        
        do {
            try task.run()
            print("✅ UI launched! Check for the FlowLoom window.")
        } catch {
            print("❌ Failed to launch UI: \(error)")
        }
        
        print("\nPress Enter to return to menu...")
        _ = readLine()
    }
    
    static func showSampleCommands() {
        print("""
        
        📋 Sample FlowLoom Commands
        ===========================
        
        Planning & Architecture:
        - plan:create "Project Name"
        - plan:review 225
        - plan:update status
        - architecture:design "Component"
        
        Code Generation:
        - code:generate "Swift REST API client"
        - code:analyze MyClass.swift
        - code:refactor "optimize performance"
        - test:create "unit tests for UserService"
        
        Documentation:
        - docs:create README
        - docs:update changelog
        - docs:generate api
        
        Memory & Context:
        - memory:add "Important decision about X"
        - memory:search "authentication"
        - context:save "current-feature"
        - context:load "previous-session"
        
        Workflow:
        - workflow:start "feature-development"
        - sync
        - review:changes
        - commit "feat: add new capability"
        
        Multi-Agent:
        - multi:assign "Frontend: create login form"
        - multi:coordinate "Build user dashboard"
        - multi:status
        
        Natural Language Examples:
        - "Create a project plan for a mobile app"
        - "Show me how to implement a singleton in Swift"
        - "Review the code I just wrote"
        - "What's the best way to handle errors?"
        - "Generate tests for the User model"
        
        Press Enter to continue...
        """)
        _ = readLine()
    }
    
    static func testInstallation() {
        print("""
        
        🔧 Testing FlowLoom UI Installation
        ===================================
        
        Checking system integration...
        
        """)
        
        // Check launcher
        if FileManager.default.fileExists(atPath: "/usr/local/bin/flowloom-ui") {
            print("✅ Launcher installed at /usr/local/bin/flowloom-ui")
        } else {
            print("❌ Launcher not found (run with sudo to install)")
        }
        
        // Check app bundle
        if FileManager.default.fileExists(atPath: "/Applications/FlowLoom.app") {
            print("✅ FlowLoom.app installed in Applications")
        } else {
            print("❌ FlowLoom.app not found")
        }
        
        // Check configuration
        let configPath = NSString(string: "~/.flowloom/config.json").expandingTildeInPath
        if FileManager.default.fileExists(atPath: configPath) {
            print("✅ Configuration found at ~/.flowloom/config.json")
        } else {
            print("❌ Configuration not found")
        }
        
        // Check permissions
        print("\nPermission Status:")
        print("- Speech Recognition: Check System Preferences")
        print("- Microphone Access: Check System Preferences")
        print("- Accessibility: Check System Preferences")
        
        print("""
        
        To install system-wide:
        sudo swift FlowLoomUI-Launcher.swift --install
        
        Press Enter to continue...
        """)
        _ = readLine()
    }
}

// MARK: - Demo Data Generator

struct DemoDataGenerator {
    static func generateProjectPlan() -> String {
        return """
        # Project Plan: AI-Powered Task Manager
        
        ## Overview
        An intelligent task management application that uses AI to help users organize, prioritize, and complete their tasks more efficiently.
        
        ## Key Features
        1. **Smart Task Creation**
           - Natural language input
           - Automatic categorization
           - Due date inference
           
        2. **AI-Powered Prioritization**
           - Context-aware ranking
           - Deadline optimization
           - Energy level matching
           
        3. **Intelligent Reminders**
           - Adaptive notification timing
           - Context-based alerts
           - Progress tracking
        
        ## Technical Architecture
        
        ```mermaid
        graph TB
            A[Flutter App] --> B[REST API]
            B --> C[AI Service]
            B --> D[PostgreSQL]
            C --> E[GPT-4 API]
            B --> F[Redis Cache]
        ```
        
        ## Timeline
        
        | Phase | Duration | Deliverables |
        |-------|----------|--------------|
        | Research | 1 week | Market analysis, User surveys |
        | Design | 2 weeks | UI/UX mockups, Architecture |
        | MVP Development | 4 weeks | Core features |
        | Testing | 1 week | QA, Beta feedback |
        | Launch | 1 week | Deployment, Marketing |
        
        ## Success Metrics
        - User engagement: 70% daily active users
        - Task completion rate: 85%
        - User satisfaction: 4.5+ stars
        """
    }
    
    static func generateSwiftCode() -> String {
        return """
        ```swift
        // AI-Powered Task Manager - Task Model
        
        import Foundation
        import SwiftUI
        
        // MARK: - Task Model
        
        struct Task: Identifiable, Codable {
            let id: UUID
            var title: String
            var description: String?
            var priority: Priority
            var status: Status
            var dueDate: Date?
            var tags: Set<String>
            var aiSuggestions: AISuggestions?
            let createdAt: Date
            var updatedAt: Date
            
            init(
                id: UUID = UUID(),
                title: String,
                description: String? = nil,
                priority: Priority = .medium,
                status: Status = .pending,
                dueDate: Date? = nil,
                tags: Set<String> = []
            ) {
                self.id = id
                self.title = title
                self.description = description
                self.priority = priority
                self.status = status
                self.dueDate = dueDate
                self.tags = tags
                self.createdAt = Date()
                self.updatedAt = Date()
            }
        }
        
        // MARK: - Supporting Types
        
        extension Task {
            enum Priority: String, CaseIterable, Codable {
                case low, medium, high, urgent
                
                var color: Color {
                    switch self {
                    case .low: return .green
                    case .medium: return .orange
                    case .high: return .red
                    case .urgent: return .purple
                    }
                }
                
                var weight: Int {
                    switch self {
                    case .low: return 1
                    case .medium: return 2
                    case .high: return 3
                    case .urgent: return 4
                    }
                }
            }
            
            enum Status: String, CaseIterable, Codable {
                case pending, inProgress, completed, cancelled
                
                var icon: String {
                    switch self {
                    case .pending: return "circle"
                    case .inProgress: return "circle.lefthalf.filled"
                    case .completed: return "checkmark.circle.fill"
                    case .cancelled: return "xmark.circle"
                    }
                }
            }
        }
        
        // MARK: - AI Suggestions
        
        struct AISuggestions: Codable {
            let suggestedPriority: Task.Priority?
            let suggestedDueDate: Date?
            let suggestedTags: Set<String>
            let timeEstimate: TimeInterval?
            let relatedTasks: [UUID]
            let completionTips: [String]
            let bestTimeToWork: DateComponents?
            
            var hasAnySuggestion: Bool {
                suggestedPriority != nil ||
                suggestedDueDate != nil ||
                !suggestedTags.isEmpty ||
                timeEstimate != nil ||
                !relatedTasks.isEmpty ||
                !completionTips.isEmpty ||
                bestTimeToWork != nil
            }
        }
        
        // MARK: - Task Manager
        
        @MainActor
        class TaskManager: ObservableObject {
            @Published var tasks: [Task] = []
            @Published var isProcessingAI = false
            
            private let aiService: AIService
            private let storage: TaskStorage
            
            init(aiService: AIService = .shared, storage: TaskStorage = .shared) {
                self.aiService = aiService
                self.storage = storage
                loadTasks()
            }
            
            func createTask(from naturalLanguage: String) async throws {
                isProcessingAI = true
                defer { isProcessingAI = false }
                
                // Parse natural language input
                let parsed = try await aiService.parseTaskInput(naturalLanguage)
                
                // Create task with AI suggestions
                var task = Task(
                    title: parsed.title,
                    description: parsed.description,
                    priority: parsed.suggestedPriority ?? .medium,
                    dueDate: parsed.suggestedDueDate,
                    tags: parsed.suggestedTags
                )
                
                // Get additional AI suggestions
                task.aiSuggestions = try await aiService.generateSuggestions(for: task)
                
                // Save task
                tasks.append(task)
                try await storage.save(task)
            }
            
            func updateTaskWithAI(_ task: Task) async throws {
                isProcessingAI = true
                defer { isProcessingAI = false }
                
                // Get fresh AI suggestions
                var updatedTask = task
                updatedTask.aiSuggestions = try await aiService.generateSuggestions(for: task)
                updatedTask.updatedAt = Date()
                
                // Update in array
                if let index = tasks.firstIndex(where: { $0.id == task.id }) {
                    tasks[index] = updatedTask
                }
                
                // Save to storage
                try await storage.save(updatedTask)
            }
            
            func completeTask(_ task: Task) async throws {
                var updatedTask = task
                updatedTask.status = .completed
                updatedTask.updatedAt = Date()
                
                // Learn from completion for future suggestions
                try await aiService.recordCompletion(task)
                
                // Update
                if let index = tasks.firstIndex(where: { $0.id == task.id }) {
                    tasks[index] = updatedTask
                }
                
                try await storage.save(updatedTask)
            }
            
            private func loadTasks() {
                Task {
                    do {
                        tasks = try await storage.loadAll()
                    } catch {
                        print("Failed to load tasks: \\(error)")
                    }
                }
            }
        }
        ```
        """
    }
}

// Run the demo
FlowLoomUIDemo.main()