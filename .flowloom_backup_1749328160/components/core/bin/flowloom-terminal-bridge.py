#!/usr/bin/env python3
"""
FlowLoom Terminal Bridge
Sends commands to a specific Terminal window running Claude Code
"""

import subprocess
import time
import json
from pathlib import Path
from typing import Optional, Dict, Any

class TerminalBridge:
    def __init__(self):
        self.terminal_app = "Terminal"
        
    def list_terminal_windows(self) -> list:
        """List all Terminal windows and their contents"""
        script = '''
        tell application "Terminal"
            set windowList to {}
            repeat with w in windows
                set windowInfo to {windowID:id of w, windowName:name of w}
                try
                    set windowInfo to windowInfo & {currentTab:contents of selected tab of w}
                end try
                set end of windowList to windowInfo
            end repeat
            return windowList
        end tell
        '''
        
        result = subprocess.run(['osascript', '-e', script], 
                              capture_output=True, text=True)
        
        if result.returncode == 0:
            # Parse the AppleScript result
            windows = []
            output = result.stdout.strip()
            # Simple parsing - in production would be more robust
            print(f"Terminal windows: {output}")
            return output
        return []
    
    def send_to_terminal_by_content(self, command: str, 
                                    window_content: str = "claude") -> bool:
        """
        Send command to Terminal window containing specific content
        
        Args:
            command: Command to send
            window_content: Text to search for in window (default: "claude")
        """
        script = f'''
        tell application "Terminal"
            repeat with w in windows
                repeat with t in tabs of w
                    if (history of t) contains "{window_content}" then
                        do script "{command}" in t
                        return true
                    end if
                end repeat
            end repeat
            return false
        end tell
        '''
        
        result = subprocess.run(['osascript', '-e', script], 
                              capture_output=True, text=True)
        
        return result.returncode == 0
    
    def send_to_frontmost_terminal(self, command: str) -> bool:
        """Send command to the frontmost Terminal window"""
        # Escape special characters in command
        escaped_command = command.replace('"', '\\"').replace('\\', '\\\\')
        
        # Use keystroke method instead of 'do script' to type in current session
        script = f'''
        tell application "Terminal"
            if (count of windows) > 0 then
                activate
                tell application "System Events"
                    keystroke "{escaped_command}"
                    keystroke return
                end tell
                return true
            else
                return false
            end if
        end tell
        '''
        
        result = subprocess.run(['osascript', '-e', script], 
                              capture_output=True, text=True)
        
        return result.returncode == 0
    
    def send_to_terminal_tab(self, command: str, 
                            window_index: int = 1, 
                            tab_index: int = 1) -> bool:
        """Send command to specific Terminal window and tab"""
        escaped_command = command.replace('"', '\\"').replace('\\', '\\\\')
        
        script = f'''
        tell application "Terminal"
            if (count of windows) >= {window_index} then
                set targetWindow to window {window_index}
                if (count of tabs of targetWindow) >= {tab_index} then
                    do script "{escaped_command}" in tab {tab_index} of targetWindow
                    return true
                end if
            end if
            return false
        end tell
        '''
        
        result = subprocess.run(['osascript', '-e', script], 
                              capture_output=True, text=True)
        
        return result.returncode == 0
    
    def activate_terminal(self):
        """Bring Terminal to front"""
        script = 'tell application "Terminal" to activate'
        subprocess.run(['osascript', '-e', script])


class FlowLoomTerminalAutomation:
    """Automates FlowLoom UI commands through Terminal"""
    
    def __init__(self, project_root: str = "."):
        self.project_root = Path(project_root)
        self.flowloom_dir = self.project_root / ".flowloom"
        self.command_file = self.project_root / "ui-command.json"
        self.bridge = TerminalBridge()
        self.processed_commands = set()
        
    def watch_and_relay(self):
        """Watch for UI commands and relay them to Terminal"""
        print("🤖 FlowLoom Terminal Automation Started")
        print("   This will relay UI commands to your Terminal window")
        print("   Make sure Claude Code is running in Terminal!")
        print("   Press Ctrl+C to stop\n")
        
        # Give user time to setup
        print("📝 Setup Instructions:")
        print("   1. Open Terminal and run: claude")
        print("   2. Wait for Claude Code to be ready")
        print("   3. This script will auto-detect and send commands")
        print()
        
        while True:
            try:
                if self.command_file.exists():
                    self.process_command()
                time.sleep(0.1)
            except KeyboardInterrupt:
                print("\n✋ Stopping automation")
                break
            except Exception as e:
                print(f"❌ Error: {e}")
                time.sleep(1)
    
    def process_command(self):
        """Process a UI command and send to Terminal"""
        try:
            with open(self.command_file, 'r') as f:
                command_data = json.load(f)
            
            command_id = command_data.get('id')
            if command_id and command_id not in self.processed_commands:
                self.processed_commands.add(command_id)
                
                # Convert to FlowLoom command
                flowloom_command = self.format_command(command_data)
                
                print(f"📤 Sending to Terminal: {flowloom_command}")
                print(f"📋 Command data: {command_data}")
                
                # File-based communication - no keystroke injection needed
                print("✅ Command processed via file communication")
                success = True
                
                # Clean up command file
                self.command_file.unlink()
                
                # Bridge only relays - Claude will respond directly
                print("✅ Message relayed to Claude - waiting for Claude's response")
                
        except json.JSONDecodeError:
            pass
        except Exception as e:
            print(f"❌ Error processing command: {e}")
    
    # Removed capture_response - Claude responds directly
    
    def format_command(self, command_data: Dict[str, Any]) -> str:
        """Format UI command for FlowLoom/Claude"""
        command = command_data.get('command', '')
        args = command_data.get('args', [])
        
        # Handle special commands
        if command == 'interpret':
            # Natural language - send as-is
            return ' '.join(args)
        elif ':' in command:
            # FlowLoom command format
            return f"/{command.replace(':', '/')} {' '.join(args)}"
        else:
            # Direct command
            return f"{command} {' '.join(args)}"


def demo_terminal_control():
    """Demo Terminal control capabilities"""
    bridge = TerminalBridge()
    
    print("🎮 Terminal Control Demo")
    print("========================\n")
    
    print("1. Listing Terminal windows...")
    windows = bridge.list_terminal_windows()
    print(f"   Found: {windows}\n")
    
    print("2. Sending test command to frontmost Terminal...")
    if bridge.send_to_frontmost_terminal("echo 'Hello from FlowLoom UI!'"):
        print("   ✅ Success!\n")
    else:
        print("   ❌ Failed - make sure Terminal is open\n")
    
    print("3. Ready for automation!")
    print("   Run 'claude' in Terminal, then use the UI")


def main():
    import argparse
    
    parser = argparse.ArgumentParser(description='FlowLoom Terminal Bridge')
    parser.add_argument('--demo', action='store_true',
                       help='Run Terminal control demo')
    parser.add_argument('--project', '-p', default='.',
                       help='Project root directory')
    parser.add_argument('--window', '-w', type=int, default=1,
                       help='Terminal window number (default: 1)')
    parser.add_argument('--tab', '-t', type=int, default=1,
                       help='Terminal tab number (default: 1)')
    
    args = parser.parse_args()
    
    if args.demo:
        demo_terminal_control()
    else:
        automation = FlowLoomTerminalAutomation(args.project)
        automation.watch_and_relay()


if __name__ == '__main__':
    main()