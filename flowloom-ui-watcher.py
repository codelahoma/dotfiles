#!/usr/bin/env python3
"""
FlowLoom UI Watcher
Watches for UI commands and generates responses by executing FlowLoom commands
"""

import json
import os
import time
import subprocess
from datetime import datetime
from pathlib import Path
from typing import Dict, Any, Optional

class FlowLoomUIWatcher:
    def __init__(self, project_root: str = "."):
        self.project_root = Path(project_root)
        self.flowloom_dir = self.project_root / ".flowloom"
        self.command_file = self.flowloom_dir / "ui-command.json"
        self.response_file = self.flowloom_dir / "ui-response.json"
        self.memory_file = self.project_root / "memory.json"
        
        # Ensure .flowloom directory exists
        self.flowloom_dir.mkdir(exist_ok=True)
        
        # Track processed commands
        self.processed_commands = set()
        
    def watch(self):
        """Watch for UI commands and process them"""
        print(f"🔍 FlowLoom UI Watcher started in {self.project_root}")
        print(f"   Watching: {self.command_file}")
        print("   Press Ctrl+C to stop\n")
        
        while True:
            try:
                if self.command_file.exists():
                    self.process_command_file()
                time.sleep(0.1)  # Check every 100ms
            except KeyboardInterrupt:
                print("\n👋 Stopping FlowLoom UI Watcher")
                break
            except Exception as e:
                print(f"❌ Error: {e}")
                time.sleep(1)  # Wait a bit on error
    
    def process_command_file(self):
        """Process a command file if it's new"""
        try:
            with open(self.command_file, 'r') as f:
                command_data = json.load(f)
            
            command_id = command_data.get('id')
            if command_id and command_id not in self.processed_commands:
                print(f"📥 Processing command: {command_id}")
                self.processed_commands.add(command_id)
                
                # Process the command
                response = self.execute_command(command_data)
                
                # Write response
                self.write_response(command_id, command_data, response)
                
                # Clean up command file
                self.command_file.unlink()
                
        except json.JSONDecodeError:
            pass  # File might be partially written
        except Exception as e:
            print(f"❌ Error processing command: {e}")
    
    def execute_command(self, command_data: Dict[str, Any]) -> Dict[str, Any]:
        """Execute a FlowLoom command and return response"""
        command = command_data.get('command', '')
        args = command_data.get('args', [])
        
        print(f"   Command: {command}")
        print(f"   Args: {args}")
        
        # Route commands to appropriate handlers
        if command == "plan:create":
            return self.handle_plan_create(args)
        elif command == "code:generate":
            return self.handle_code_generate(args)
        elif command == "interpret":
            return self.handle_interpret(args)
        elif command == "memory:search":
            return self.handle_memory_search(args)
        else:
            # Try to execute as Claude command
            return self.execute_claude_command(command, args)
    
    def handle_plan_create(self, args: list) -> Dict[str, Any]:
        """Handle plan creation command"""
        project_name = args[0] if args else "New Project"
        
        # Execute FlowLoom plan command through Claude
        claude_command = f"/project:plan:create {project_name}"
        result = self.run_claude(claude_command)
        
        if result['success']:
            return {
                'presentation': result['output'],
                'conversation': f"Created project plan for '{project_name}'",
                'format': 'markdown'
            }
        else:
            return {
                'presentation': None,
                'conversation': f"Failed to create plan: {result['error']}",
                'format': 'error'
            }
    
    def handle_code_generate(self, args: list) -> Dict[str, Any]:
        """Handle code generation command"""
        description = ' '.join(args)
        
        claude_command = f"Generate code for: {description}"
        result = self.run_claude(claude_command)
        
        if result['success']:
            # Try to detect language from output
            output = result['output']
            language = 'text'
            if '```swift' in output.lower():
                language = 'swift'
            elif '```python' in output.lower():
                language = 'python'
            elif '```javascript' in output.lower() or '```js' in output.lower():
                language = 'javascript'
            
            return {
                'presentation': output,
                'conversation': f"Generated code for: {description}",
                'format': f'code:{language}'
            }
        else:
            return {
                'presentation': None,
                'conversation': f"Code generation failed: {result['error']}",
                'format': 'error'
            }
    
    def handle_interpret(self, args: list) -> Dict[str, Any]:
        """Handle natural language interpretation"""
        query = ' '.join(args)
        
        # First check if it matches a FlowLoom command pattern
        if 'plan' in query.lower():
            return self.handle_plan_create([query])
        elif 'code' in query.lower():
            return self.handle_code_generate([query])
        else:
            # Pass through to Claude
            result = self.run_claude(query)
            
            return {
                'presentation': result.get('output') if len(result.get('output', '')) > 200 else None,
                'conversation': result.get('output') if len(result.get('output', '')) <= 200 else "Response displayed in presentation pane",
                'format': 'markdown'
            }
    
    def handle_memory_search(self, args: list) -> Dict[str, Any]:
        """Search memory.json for relevant entries"""
        query = ' '.join(args).lower()
        
        try:
            with open(self.memory_file, 'r') as f:
                memory_data = json.load(f)
            
            entries = memory_data.get('entries', [])
            matches = []
            
            for entry in entries:
                # Search in various fields
                if any(query in str(v).lower() for v in entry.values()):
                    matches.append(entry)
            
            if matches:
                # Format matches as markdown
                presentation = f"# Memory Search Results: '{query}'\n\n"
                presentation += f"Found {len(matches)} matches:\n\n"
                
                for i, match in enumerate(matches[:10], 1):  # Limit to 10
                    presentation += f"## Match {i}\n"
                    presentation += f"- **Type**: {match.get('type', 'unknown')}\n"
                    presentation += f"- **Timestamp**: {match.get('timestamp', 'unknown')}\n"
                    
                    # Show relevant content
                    for key, value in match.items():
                        if key not in ['type', 'timestamp'] and query in str(value).lower():
                            presentation += f"- **{key.title()}**: {value}\n"
                    presentation += "\n"
                
                return {
                    'presentation': presentation,
                    'conversation': f"Found {len(matches)} memory entries matching '{query}'",
                    'format': 'markdown'
                }
            else:
                return {
                    'presentation': None,
                    'conversation': f"No memory entries found matching '{query}'",
                    'format': 'plain'
                }
                
        except Exception as e:
            return {
                'presentation': None,
                'conversation': f"Memory search failed: {e}",
                'format': 'error'
            }
    
    def execute_claude_command(self, command: str, args: list) -> Dict[str, Any]:
        """Execute command through Claude"""
        # Convert FlowLoom command format to Claude slash command
        claude_command = f"/{command.replace(':', '/')} {' '.join(args)}"
        result = self.run_claude(claude_command)
        
        return {
            'presentation': result.get('output'),
            'conversation': f"Executed: {command}",
            'format': 'markdown'
        }
    
    def run_claude(self, command: str) -> Dict[str, Any]:
        """Run a command through Claude CLI"""
        try:
            # Check if claude is available
            claude_path = subprocess.run(['which', 'claude'], 
                                       capture_output=True, 
                                       text=True).stdout.strip()
            
            if not claude_path:
                return {
                    'success': False,
                    'error': 'Claude CLI not found',
                    'output': None
                }
            
            # Run the command
            result = subprocess.run(
                ['claude', command],
                capture_output=True,
                text=True,
                timeout=30
            )
            
            if result.returncode == 0:
                return {
                    'success': True,
                    'output': result.stdout,
                    'error': None
                }
            else:
                return {
                    'success': False,
                    'output': None,
                    'error': result.stderr or 'Command failed'
                }
                
        except subprocess.TimeoutExpired:
            return {
                'success': False,
                'output': None,
                'error': 'Command timed out'
            }
        except Exception as e:
            return {
                'success': False,
                'output': None,
                'error': str(e)
            }
    
    def write_response(self, command_id: str, command_data: Dict[str, Any], 
                      response: Dict[str, Any]):
        """Write response to file"""
        response_data = {
            'commandId': command_id,
            'command': command_data.get('command'),
            'presentation': response.get('presentation'),
            'conversation': response.get('conversation'),
            'format': response.get('format', 'plain'),
            'executionTime': 0.5,  # Placeholder
            'planContext': None,
            'timestamp': datetime.now().isoformat()
        }
        
        with open(self.response_file, 'w') as f:
            json.dump(response_data, f, indent=2)
        
        print(f"   ✅ Response written to {self.response_file}")
        
        # Also append to memory for persistence
        self.append_to_memory({
            'type': 'ui_response',
            'commandId': command_id,
            'command': command_data.get('command'),
            'presentation': response.get('presentation'),
            'conversation': response.get('conversation'),
            'format': response.get('format', 'plain'),
            'timestamp': datetime.now().isoformat(),
            'session': command_data.get('sessionId')
        })
    
    def append_to_memory(self, entry: Dict[str, Any]):
        """Append an entry to memory.json"""
        try:
            # Read existing memory
            memory_data = {}
            if self.memory_file.exists():
                with open(self.memory_file, 'r') as f:
                    memory_data = json.load(f)
            
            # Ensure entries list exists
            if 'entries' not in memory_data:
                memory_data['entries'] = []
            
            # Append new entry
            memory_data['entries'].append(entry)
            memory_data['lastUpdated'] = datetime.now().isoformat()
            
            # Write back
            with open(self.memory_file, 'w') as f:
                json.dump(memory_data, f, indent=2)
                
        except Exception as e:
            print(f"⚠️  Failed to update memory: {e}")


def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description='FlowLoom UI Watcher')
    parser.add_argument('--project', '-p', default='.', 
                       help='Project root directory (default: current directory)')
    parser.add_argument('--test', action='store_true',
                       help='Run in test mode with sample responses')
    
    args = parser.parse_args()
    
    watcher = FlowLoomUIWatcher(args.project)
    
    if args.test:
        print("🧪 Running in test mode - will generate sample responses")
        # In test mode, just generate mock responses
    
    try:
        watcher.watch()
    except KeyboardInterrupt:
        print("\n✋ Stopped by user")
    except Exception as e:
        print(f"❌ Fatal error: {e}")
        return 1
    
    return 0


if __name__ == '__main__':
    exit(main())