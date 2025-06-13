#!/usr/bin/env python3
"""
Append Content to Second Screen Display
"""

import os
import sys
import subprocess
import datetime
import argparse
from pathlib import Path

def get_shell_pid():
    """Get the proper shell PID for display file naming."""
    try:
        result = subprocess.run(['./.flowloom/bin/get_shell_pid.sh'], 
                              capture_output=True, text=True, check=True)
        return result.stdout.strip()
    except (subprocess.CalledProcessError, FileNotFoundError):
        return str(os.getpid())

def get_display_path():
    """Get the path for the current display file."""
    shell_pid = get_shell_pid()
    display_file = f"claude_display_{shell_pid}.md"
    return Path.cwd() / '.flowloom' / 'displays' / display_file

def append_to_display(content):
    """Append content to the display."""
    display_path = get_display_path()
    
    if not display_path.exists():
        print(f"❌ No active display found for PID {get_shell_pid()}")
        print("💡 Create one with: ./.flowloom/bin/second-screen")
        return False
    
    # Read current content
    with open(display_path, 'r') as f:
        current_content = f.read()
    
    # Remove the last updated line
    lines = current_content.split('\n')
    if lines and lines[-1].startswith('*Last updated:'):
        lines = lines[:-1]
    
    # Add new content and timestamp
    timestamp = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    lines.extend(['', content, '', '---', '', f'*Last updated: {timestamp}*'])
    
    # Write back to file
    with open(display_path, 'w') as f:
        f.write('\n'.join(lines))
    
    print("✅ Content appended to display")
    print(f"📍 Path: {display_path}")
    return True

def main():
    parser = argparse.ArgumentParser(description='Append content to second screen display')
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('content', nargs='?', help='Content to append')
    group.add_argument('--from-file', '-f', help='Read content from file')
    
    args = parser.parse_args()
    
    if args.from_file:
        try:
            with open(args.from_file, 'r') as f:
                content = f.read()
        except FileNotFoundError:
            print(f"❌ File not found: {args.from_file}")
            return 1
        except Exception as e:
            print(f"❌ Error reading file: {e}")
            return 1
    else:
        content = args.content
    
    success = append_to_display(content)
    return 0 if success else 1

if __name__ == "__main__":
    sys.exit(main())