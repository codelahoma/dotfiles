import SwiftUI
import AppKit

// MARK: - Models

struct ActivityEntry: Identifiable {
    let id = UUID()
    let timestamp: Date
    let type: ActivityType
    let message: String
    
    enum ActivityType {
        case command, response, system, error, info
        
        var color: Color {
            switch self {
            case .command: return .blue
            case .response: return .green
            case .system: return .orange
            case .error: return .red
            case .info: return Color(NSColor.secondaryLabelColor)
            }
        }
        
        var icon: String {
            switch self {
            case .command: return "arrow.up.circle"
            case .response: return "arrow.down.circle"
            case .system: return "gear"
            case .error: return "exclamationmark.triangle"
            case .info: return "info.circle"
            }
        }
    }
}

// MARK: - Bottom Panel View

struct BottomPanelView: View {
    @ObservedObject var viewModel: FlowLoomViewModel
    @State private var inputText = ""
    @FocusState private var inputFocused: Bool
    @State private var panelWidths: [CGFloat] = [0.33, 0.34, 0.33]
    @State private var commandHistoryIndex = -1
    
    var body: some View {
        GeometryReader { geometry in
            HStack(spacing: 0) {
                // Input Panel
                InputPanel(
                    text: $inputText,
                    viewModel: viewModel,
                    focused: $inputFocused,
                    historyIndex: $commandHistoryIndex
                )
                .frame(width: geometry.size.width * panelWidths[0])
                
                PanelDivider()
                
                // Activity Panel
                ActivityPanel(viewModel: viewModel)
                    .frame(width: geometry.size.width * panelWidths[1])
                
                PanelDivider()
                
                // System Status Panel
                SystemStatusPanel(viewModel: viewModel)
                    .frame(width: geometry.size.width * panelWidths[2])
            }
        }
        .frame(height: 150)
        .background(Color(NSColor.windowBackgroundColor))
        .overlay(
            Rectangle()
                .frame(height: 0.5)
                .foregroundColor(Color(NSColor.separatorColor)),
            alignment: .top
        )
    }
}

// MARK: - Input Panel

struct InputPanel: View {
    @Binding var text: String
    @ObservedObject var viewModel: FlowLoomViewModel
    @FocusState.Binding var focused: Bool
    @Binding var historyIndex: Int
    
    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            // Header
            HStack {
                Label("Input", systemImage: "keyboard")
                    .font(.caption)
                    .foregroundColor(.secondary)
                Spacer()
                if !viewModel.commandHistory.isEmpty {
                    Text("\(historyIndex + 1)/\(viewModel.commandHistory.count)")
                        .font(.caption2)
                        .foregroundColor(.tertiary)
                }
            }
            .padding(.horizontal, 12)
            .padding(.top, 8)
            .padding(.bottom, 4)
            
            // Input field
            VStack(spacing: 4) {
                TextField("Enter command or query...", text: $text)
                    .textFieldStyle(.plain)
                    .font(.system(.body, design: .monospaced))
                    .padding(10)
                    .background(Color(NSColor.controlBackgroundColor))
                    .cornerRadius(6)
                    .overlay(
                        RoundedRectangle(cornerRadius: 6)
                            .stroke(focused ? Color.accentColor : Color.clear, lineWidth: 2)
                    )
                    .focused($focused)
                    .onSubmit {
                        submitCommand()
                    }
                    .onKeyPress(keys: [.upArrow, .downArrow]) { press in
                        handleHistoryNavigation(press.key)
                        return .handled
                    }
                
                // Command suggestions
                if focused && !viewModel.commandSuggestions.isEmpty {
                    ScrollView(.horizontal, showsIndicators: false) {
                        HStack(spacing: 6) {
                            ForEach(viewModel.commandSuggestions, id: \.self) { suggestion in
                                CommandSuggestionChip(
                                    text: suggestion,
                                    action: {
                                        text = suggestion
                                        focused = true
                                    }
                                )
                            }
                        }
                    }
                    .padding(.horizontal, 2)
                    .frame(height: 24)
                }
            }
            .padding(.horizontal, 12)
            .padding(.bottom, 8)
            
            Spacer(minLength: 0)
        }
        .background(Color(NSColor.windowBackgroundColor))
    }
    
    private func submitCommand() {
        guard !text.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty else { return }
        
        viewModel.processInput(text)
        viewModel.commandHistory.insert(text, at: 0)
        text = ""
        historyIndex = -1
    }
    
    private func handleHistoryNavigation(_ key: KeyEquivalent) {
        guard !viewModel.commandHistory.isEmpty else { return }
        
        switch key {
        case .upArrow:
            if historyIndex < viewModel.commandHistory.count - 1 {
                historyIndex += 1
                text = viewModel.commandHistory[historyIndex]
            }
        case .downArrow:
            if historyIndex > -1 {
                historyIndex -= 1
                text = historyIndex >= 0 ? viewModel.commandHistory[historyIndex] : ""
            }
        default:
            break
        }
    }
}

// MARK: - Activity Panel

struct ActivityPanel: View {
    @ObservedObject var viewModel: FlowLoomViewModel
    @State private var autoScroll = true
    
    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            // Header
            HStack {
                Label("Activity", systemImage: "waveform")
                    .font(.caption)
                    .foregroundColor(.secondary)
                Spacer()
                Toggle("Auto-scroll", isOn: $autoScroll)
                    .toggleStyle(.switch)
                    .controlSize(.mini)
                    .labelsHidden()
                Menu {
                    Button("Clear All") {
                        viewModel.activityLog.removeAll()
                    }
                    Divider()
                    Button("Export Log...") {
                        exportActivityLog()
                    }
                } label: {
                    Image(systemName: "ellipsis.circle")
                        .foregroundColor(.secondary)
                }
                .menuStyle(.borderlessButton)
                .frame(width: 16, height: 16)
            }
            .padding(.horizontal, 12)
            .padding(.top, 8)
            .padding(.bottom, 4)
            
            // Activity list
            ScrollViewReader { proxy in
                ScrollView {
                    LazyVStack(alignment: .leading, spacing: 2) {
                        ForEach(viewModel.activityLog) { entry in
                            ActivityEntryView(entry: entry)
                                .id(entry.id)
                        }
                        
                        if viewModel.activityLog.isEmpty {
                            Text("No activity yet")
                                .font(.caption)
                                .foregroundColor(.tertiary)
                                .frame(maxWidth: .infinity)
                                .padding(.vertical, 20)
                        }
                    }
                    .padding(.horizontal, 12)
                }
                .onChange(of: viewModel.activityLog.count) { _ in
                    if autoScroll, let last = viewModel.activityLog.last {
                        withAnimation(.easeOut(duration: 0.2)) {
                            proxy.scrollTo(last.id, anchor: .bottom)
                        }
                    }
                }
            }
        }
        .background(Color(NSColor.windowBackgroundColor))
    }
    
    private func exportActivityLog() {
        let savePanel = NSSavePanel()
        savePanel.allowedContentTypes = [.plainText]
        savePanel.nameFieldStringValue = "flowloom-activity-\(Date().ISO8601Format()).txt"
        
        if savePanel.runModal() == .OK, let url = savePanel.url {
            let logContent = viewModel.activityLog.map { entry in
                "[\(entry.timestamp.ISO8601Format())] \(entry.type): \(entry.message)"
            }.joined(separator: "\n")
            
            try? logContent.write(to: url, atomically: true, encoding: .utf8)
        }
    }
}

// MARK: - System Status Panel

struct SystemStatusPanel: View {
    @ObservedObject var viewModel: FlowLoomViewModel
    
    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            // Header
            HStack {
                Label("System Status", systemImage: "info.circle")
                    .font(.caption)
                    .foregroundColor(.secondary)
                Spacer()
            }
            .padding(.horizontal, 12)
            .padding(.top, 8)
            .padding(.bottom, 4)
            
            // Status items
            VStack(alignment: .leading, spacing: 6) {
                StatusRow(
                    icon: "network",
                    label: "Connection",
                    value: viewModel.connectionStatus.displayText,
                    color: viewModel.connectionStatus.color
                )
                
                StatusRow(
                    icon: "memorychip",
                    label: "Memory",
                    value: viewModel.memoryStatus,
                    color: .green
                )
                
                StatusRow(
                    icon: "tag",
                    label: "Session",
                    value: String(viewModel.sessionId.prefix(8)),
                    color: .blue
                )
                
                StatusRow(
                    icon: "record.circle",
                    label: "Auto-Track",
                    value: viewModel.autoTrackStatus ? "Active" : "Inactive",
                    color: viewModel.autoTrackStatus ? .green : .gray
                )
                
                StatusRow(
                    icon: "speedometer",
                    label: "Performance",
                    value: viewModel.performanceMetric,
                    color: .orange
                )
                
                if let error = viewModel.lastError {
                    Divider().padding(.vertical, 4)
                    
                    StatusRow(
                        icon: "exclamationmark.triangle",
                        label: "Error",
                        value: error,
                        color: .red
                    )
                    .lineLimit(2)
                }
            }
            .padding(.horizontal, 12)
            .padding(.bottom, 8)
            
            Spacer(minLength: 0)
        }
        .background(Color(NSColor.windowBackgroundColor))
    }
}

// MARK: - Supporting Views

struct ActivityEntryView: View {
    let entry: ActivityEntry
    
    var body: some View {
        HStack(alignment: .top, spacing: 6) {
            Text(entry.timestamp, style: .time)
                .font(.system(.caption2, design: .monospaced))
                .foregroundColor(.secondary)
                .frame(width: 50, alignment: .trailing)
            
            Image(systemName: entry.type.icon)
                .font(.caption2)
                .foregroundColor(entry.type.color)
                .frame(width: 14)
            
            Text(entry.message)
                .font(.caption)
                .foregroundColor(.primary)
                .lineLimit(3)
                .frame(maxWidth: .infinity, alignment: .leading)
        }
        .padding(.vertical, 3)
        .padding(.horizontal, 4)
        .background(
            RoundedRectangle(cornerRadius: 4)
                .fill(entry.type.color.opacity(0.05))
        )
    }
}

struct StatusRow: View {
    let icon: String
    let label: String
    let value: String
    var color: Color = .primary
    
    var body: some View {
        HStack(spacing: 8) {
            Image(systemName: icon)
                .font(.caption)
                .foregroundColor(color)
                .frame(width: 16)
            
            Text(label + ":")
                .font(.caption)
                .foregroundColor(.secondary)
            
            Text(value)
                .font(.system(.caption, design: .monospaced))
                .fontWeight(.medium)
                .foregroundColor(color)
                .lineLimit(1)
            
            Spacer()
        }
    }
}

struct CommandSuggestionChip: View {
    let text: String
    let action: () -> Void
    
    var body: some View {
        Button(action: action) {
            Text(text)
                .font(.caption)
                .padding(.horizontal, 8)
                .padding(.vertical, 2)
                .background(Color.accentColor.opacity(0.2))
                .cornerRadius(10)
        }
        .buttonStyle(.plain)
    }
}

struct PanelDivider: View {
    var body: some View {
        Rectangle()
            .fill(Color(NSColor.separatorColor))
            .frame(width: 0.5)
            .padding(.vertical, 8)
    }
}

// MARK: - View Model Extensions

extension FlowLoomViewModel {
    // Activity logging
    func logActivity(_ type: ActivityEntry.ActivityType, _ message: String) {
        let entry = ActivityEntry(timestamp: Date(), type: type, message: message)
        DispatchQueue.main.async {
            self.activityLog.append(entry)
            // Keep only last 1000 entries
            if self.activityLog.count > 1000 {
                self.activityLog.removeFirst()
            }
        }
    }
    
    // Process user input
    func processInput(_ input: String) {
        logActivity(.command, input)
        // Process the command...
    }
}

// MARK: - Connection Status Extension

extension FlowLoomViewModel.ConnectionStatus {
    var displayText: String {
        switch self {
        case .connected: return "Connected"
        case .connecting: return "Connecting..."
        case .disconnected: return "Disconnected"
        case .error(let msg): return "Error: \(msg)"
        }
    }
    
    var color: Color {
        switch self {
        case .connected: return .green
        case .connecting: return .orange
        case .disconnected: return .gray
        case .error: return .red
        }
    }
}