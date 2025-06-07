#!/usr/bin/env swift

import Foundation
import AppKit
import Security

// MARK: - FlowLoom UI Launcher
// This script sets up and launches the FlowLoom native UI with proper environment configuration

struct FlowLoomLauncher {
    static let flowloomPath = "/usr/local/bin/flowloom"
    static let claudePath = "/usr/local/bin/claude"
    static let configPath = "~/.flowloom/config.json"
    
    enum LaunchMode {
        case production  // Full FlowLoom integration
        case demo       // Mock implementation
        case hybrid     // FlowLoom commands + mock Claude
    }
    
    static func main() {
        print("🚀 FlowLoom UI Launcher")
        print("======================")
        
        // Check environment
        let mode = detectLaunchMode()
        print("Launch mode: \(mode)")
        
        // Setup environment
        setupEnvironment(mode: mode)
        
        // Check permissions
        if !checkPermissions() {
            requestPermissions()
        }
        
        // Launch UI
        launchUI(mode: mode)
    }
    
    static func detectLaunchMode() -> LaunchMode {
        let hasFlowLoom = FileManager.default.fileExists(atPath: flowloomPath)
        let hasClaude = FileManager.default.fileExists(atPath: claudePath)
        
        if hasFlowLoom && hasClaude {
            print("✅ FlowLoom and Claude detected")
            return .production
        } else if hasFlowLoom {
            print("⚠️  FlowLoom detected, Claude missing")
            return .hybrid
        } else {
            print("ℹ️  Running in demo mode")
            return .demo
        }
    }
    
    static func setupEnvironment(mode: LaunchMode) {
        // Set FlowLoom environment variables
        setenv("FLOWLOOM_UI_MODE", String(describing: mode), 1)
        setenv("FLOWLOOM_SESSION_ID", UUID().uuidString, 1)
        
        // Create config directory if needed
        let configDir = NSString(string: "~/.flowloom").expandingTildeInPath
        if !FileManager.default.fileExists(atPath: configDir) {
            try? FileManager.default.createDirectory(
                atPath: configDir,
                withIntermediateDirectories: true
            )
        }
        
        // Load or create configuration
        loadOrCreateConfig()
    }
    
    static func loadOrCreateConfig() {
        let configFile = NSString(string: configPath).expandingTildeInPath
        
        if !FileManager.default.fileExists(atPath: configFile) {
            // Create default configuration
            let defaultConfig = """
            {
                "ui": {
                    "theme": "auto",
                    "voice": {
                        "enabled": true,
                        "wakeWord": "hey flowloom",
                        "speechRate": 0.5,
                        "voiceId": "com.apple.speech.synthesis.voice.samantha"
                    },
                    "layout": {
                        "interactionWidth": 400,
                        "presentationHeightRatio": 0.67
                    },
                    "shortcuts": {
                        "commandPalette": "cmd+k",
                        "toggleVoice": "cmd+shift+v",
                        "clearChat": "cmd+shift+c"
                    }
                },
                "bridge": {
                    "flowloomPath": "\(flowloomPath)",
                    "claudePath": "\(claudePath)",
                    "timeout": 30,
                    "retries": 3
                },
                "memory": {
                    "historyLimit": 1000,
                    "autoSave": true,
                    "syncInterval": 300
                }
            }
            """
            
            try? defaultConfig.write(
                toFile: configFile,
                atomically: true,
                encoding: .utf8
            )
            
            print("📝 Created default configuration at \(configFile)")
        } else {
            print("📂 Loaded configuration from \(configFile)")
        }
    }
    
    static func checkPermissions() -> Bool {
        // Check for required permissions
        var hasAllPermissions = true
        
        // Speech recognition permission
        let speechStatus = SFSpeechRecognizer.authorizationStatus()
        if speechStatus != .authorized {
            print("❌ Speech recognition not authorized")
            hasAllPermissions = false
        }
        
        // Microphone permission
        let micStatus = AVCaptureDevice.authorizationStatus(for: .audio)
        if micStatus != .authorized {
            print("❌ Microphone access not authorized")
            hasAllPermissions = false
        }
        
        // Accessibility permission (for global shortcuts)
        let options = [kAXTrustedCheckOptionPrompt.takeUnretainedValue() as String: false]
        let accessibilityEnabled = AXIsProcessTrustedWithOptions(options as CFDictionary)
        if !accessibilityEnabled {
            print("❌ Accessibility access not authorized")
            hasAllPermissions = false
        }
        
        return hasAllPermissions
    }
    
    static func requestPermissions() {
        print("\n⚠️  FlowLoom UI requires the following permissions:")
        print("  1. Speech Recognition - for voice commands")
        print("  2. Microphone Access - for voice input")
        print("  3. Accessibility - for global keyboard shortcuts")
        print("\nPlease grant these permissions in System Preferences.")
        
        // Open System Preferences
        if let url = URL(string: "x-apple.systempreferences:com.apple.preference.security?Privacy") {
            NSWorkspace.shared.open(url)
        }
        
        print("\nPress Enter to continue after granting permissions...")
        _ = readLine()
    }
    
    static func launchUI(mode: LaunchMode) {
        print("\n🎯 Launching FlowLoom UI...")
        
        // Get the directory containing this script
        let scriptPath = CommandLine.arguments[0]
        let scriptURL = URL(fileURLWithPath: scriptPath)
        let directory = scriptURL.deletingLastPathComponent().path
        
        // Look for UI implementation
        let uiPaths = [
            "\(directory)/FlowLoomUI-Enhanced.swift",
            "\(directory)/FlowLoomUI.swift",
            "./FlowLoomUI-Enhanced.swift",
            "./FlowLoomUI.swift"
        ]
        
        var uiPath: String?
        for path in uiPaths {
            if FileManager.default.fileExists(atPath: path) {
                uiPath = path
                break
            }
        }
        
        guard let foundPath = uiPath else {
            print("❌ Could not find FlowLoom UI implementation")
            print("   Searched in: \(uiPaths)")
            exit(1)
        }
        
        print("✅ Found UI at: \(foundPath)")
        
        // Launch the UI
        let task = Process()
        task.executableURL = URL(fileURLWithPath: "/usr/bin/swift")
        task.arguments = [foundPath]
        
        // Pass environment to UI
        var environment = ProcessInfo.processInfo.environment
        environment["FLOWLOOM_LAUNCHER_VERSION"] = "1.0"
        environment["FLOWLOOM_LAUNCH_MODE"] = String(describing: mode)
        task.environment = environment
        
        do {
            try task.run()
            print("✅ FlowLoom UI launched successfully")
            
            // Keep launcher alive to monitor UI
            task.waitUntilExit()
            
            let status = task.terminationStatus
            if status != 0 {
                print("⚠️  UI exited with status: \(status)")
            }
        } catch {
            print("❌ Failed to launch UI: \(error)")
            exit(1)
        }
    }
}

// MARK: - Installation Helper

struct FlowLoomInstaller {
    static func install() {
        print("\n📦 FlowLoom UI Installation")
        print("===========================")
        
        // Check if running with sufficient permissions
        if geteuid() != 0 {
            print("⚠️  Installation requires administrator privileges")
            print("   Please run: sudo swift FlowLoomUI-Launcher.swift --install")
            exit(1)
        }
        
        installLauncherScript()
        installDesktopApp()
        setupLaunchAgent()
        
        print("\n✅ Installation complete!")
        print("   You can now:")
        print("   - Run 'flowloom-ui' from Terminal")
        print("   - Launch 'FlowLoom' from Applications")
        print("   - Use Spotlight to search for 'FlowLoom'")
    }
    
    static func installLauncherScript() {
        print("\n📝 Installing launcher script...")
        
        let launcherScript = """
        #!/bin/bash
        # FlowLoom UI Launcher
        
        SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
        
        # Find Swift implementation
        if [ -f "$HOME/.flowloom/FlowLoomUI-Enhanced.swift" ]; then
            UI_PATH="$HOME/.flowloom/FlowLoomUI-Enhanced.swift"
        elif [ -f "/Applications/FlowLoom.app/Contents/Resources/FlowLoomUI-Enhanced.swift" ]; then
            UI_PATH="/Applications/FlowLoom.app/Contents/Resources/FlowLoomUI-Enhanced.swift"
        else
            echo "❌ FlowLoom UI not found"
            exit 1
        fi
        
        # Launch with environment
        export FLOWLOOM_UI_MODE="production"
        exec /usr/bin/swift "$UI_PATH" "$@"
        """
        
        let scriptPath = "/usr/local/bin/flowloom-ui"
        
        do {
            try launcherScript.write(toFile: scriptPath, atomically: true, encoding: .utf8)
            
            // Make executable
            let attributes = [FileAttributeKey.posixPermissions: 0o755]
            try FileManager.default.setAttributes(attributes, ofItemAtPath: scriptPath)
            
            print("✅ Installed launcher at \(scriptPath)")
        } catch {
            print("❌ Failed to install launcher: \(error)")
        }
    }
    
    static func installDesktopApp() {
        print("\n🖥  Creating desktop application...")
        
        let appPath = "/Applications/FlowLoom.app"
        let contentsPath = "\(appPath)/Contents"
        let macOSPath = "\(contentsPath)/MacOS"
        let resourcesPath = "\(contentsPath)/Resources"
        
        do {
            // Create app bundle structure
            try FileManager.default.createDirectory(
                atPath: macOSPath,
                withIntermediateDirectories: true
            )
            try FileManager.default.createDirectory(
                atPath: resourcesPath,
                withIntermediateDirectories: true
            )
            
            // Create Info.plist
            let infoPlist = """
            <?xml version="1.0" encoding="UTF-8"?>
            <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
            <plist version="1.0">
            <dict>
                <key>CFBundleExecutable</key>
                <string>FlowLoom</string>
                <key>CFBundleIconFile</key>
                <string>AppIcon</string>
                <key>CFBundleIdentifier</key>
                <string>dev.rodk.flowloom</string>
                <key>CFBundleName</key>
                <string>FlowLoom</string>
                <key>CFBundlePackageType</key>
                <string>APPL</string>
                <key>CFBundleShortVersionString</key>
                <string>1.0</string>
                <key>CFBundleVersion</key>
                <string>1</string>
                <key>LSMinimumSystemVersion</key>
                <string>12.0</string>
                <key>NSHighResolutionCapable</key>
                <true/>
                <key>NSMicrophoneUsageDescription</key>
                <string>FlowLoom needs microphone access for voice commands</string>
                <key>NSSpeechRecognitionUsageDescription</key>
                <string>FlowLoom needs speech recognition for voice commands</string>
            </dict>
            </plist>
            """
            
            try infoPlist.write(
                toFile: "\(contentsPath)/Info.plist",
                atomically: true,
                encoding: .utf8
            )
            
            // Create executable
            let executable = """
            #!/bin/bash
            exec /usr/local/bin/flowloom-ui "$@"
            """
            
            let execPath = "\(macOSPath)/FlowLoom"
            try executable.write(toFile: execPath, atomically: true, encoding: .utf8)
            
            let attributes = [FileAttributeKey.posixPermissions: 0o755]
            try FileManager.default.setAttributes(attributes, ofItemAtPath: execPath)
            
            // Copy UI implementation to Resources
            let sourcePath = CommandLine.arguments[0]
            let sourceDir = URL(fileURLWithPath: sourcePath).deletingLastPathComponent().path
            
            if let uiPath = findUIImplementation(in: sourceDir) {
                let destPath = "\(resourcesPath)/FlowLoomUI-Enhanced.swift"
                try FileManager.default.copyItem(atPath: uiPath, toPath: destPath)
            }
            
            // Create a simple icon (in production, use a proper icon)
            createAppIcon(at: "\(resourcesPath)/AppIcon.icns")
            
            print("✅ Created FlowLoom.app")
        } catch {
            print("❌ Failed to create app: \(error)")
        }
    }
    
    static func findUIImplementation(in directory: String) -> String? {
        let paths = [
            "\(directory)/FlowLoomUI-Enhanced.swift",
            "\(directory)/FlowLoomUI.swift"
        ]
        
        for path in paths {
            if FileManager.default.fileExists(atPath: path) {
                return path
            }
        }
        
        return nil
    }
    
    static func createAppIcon(at path: String) {
        // Create a simple procedural icon for FlowLoom
        let size = CGSize(width: 512, height: 512)
        
        guard let image = NSImage(size: size, flipped: false) { context in
            // Background gradient
            let gradient = NSGradient(
                colors: [
                    NSColor(red: 0.2, green: 0.6, blue: 1.0, alpha: 1.0),
                    NSColor(red: 0.1, green: 0.3, blue: 0.8, alpha: 1.0)
                ]
            )
            gradient?.draw(in: NSRect(origin: .zero, size: size), angle: -45)
            
            // FlowLoom "FL" text
            let paragraphStyle = NSMutableParagraphStyle()
            paragraphStyle.alignment = .center
            
            let attributes: [NSAttributedString.Key: Any] = [
                .font: NSFont.systemFont(ofSize: 200, weight: .bold),
                .foregroundColor: NSColor.white,
                .paragraphStyle: paragraphStyle
            ]
            
            let text = "FL"
            let textSize = text.size(withAttributes: attributes)
            let textRect = NSRect(
                x: (size.width - textSize.width) / 2,
                y: (size.height - textSize.height) / 2,
                width: textSize.width,
                height: textSize.height
            )
            
            text.draw(in: textRect, withAttributes: attributes)
            
            return true
        } else {
            return
        }
        
        // Save as ICNS (simplified - in production use proper icon generation)
        if let tiffData = image.tiffRepresentation,
           let bitmap = NSBitmapImageRep(data: tiffData),
           let pngData = bitmap.representation(using: .png, properties: [:]) {
            try? pngData.write(to: URL(fileURLWithPath: path))
        }
    }
    
    static func setupLaunchAgent() {
        print("\n🚀 Setting up launch agent...")
        
        let plistPath = "\(NSHomeDirectory())/Library/LaunchAgents/dev.rodk.flowloom.plist"
        
        let launchAgent = """
        <?xml version="1.0" encoding="UTF-8"?>
        <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
        <plist version="1.0">
        <dict>
            <key>Label</key>
            <string>dev.rodk.flowloom</string>
            <key>ProgramArguments</key>
            <array>
                <string>/usr/local/bin/flowloom-ui</string>
                <string>--background</string>
            </array>
            <key>RunAtLoad</key>
            <false/>
            <key>KeepAlive</key>
            <false/>
            <key>StandardErrorPath</key>
            <string>/tmp/flowloom-ui.err</string>
            <key>StandardOutPath</key>
            <string>/tmp/flowloom-ui.out</string>
        </dict>
        </plist>
        """
        
        do {
            try launchAgent.write(toFile: plistPath, atomically: true, encoding: .utf8)
            print("✅ Created launch agent")
            print("   To enable auto-start: launchctl load \(plistPath)")
        } catch {
            print("⚠️  Failed to create launch agent: \(error)")
        }
    }
}

// MARK: - Main Entry Point

// Parse command line arguments
if CommandLine.arguments.contains("--install") {
    FlowLoomInstaller.install()
} else if CommandLine.arguments.contains("--help") {
    print("""
    FlowLoom UI Launcher
    
    Usage:
        swift FlowLoomUI-Launcher.swift [options]
    
    Options:
        --install    Install FlowLoom UI system-wide
        --help       Show this help message
    
    Without options, launches FlowLoom UI directly.
    """)
} else {
    FlowLoomLauncher.main()
}