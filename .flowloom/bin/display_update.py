#!/usr/bin/env python3
"""
Update Second Screen Display Content
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

def update_display(content):
    """Update the display with new content."""
    display_path = get_display_path()
    
    if not display_path.exists():
        print(f"❌ No active display found for PID {get_shell_pid()}")
        print("💡 Create one with: ./.flowloom/bin/second-screen")
        return False
    
    shell_pid = get_shell_pid()
    timestamp = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    
    display_content = f"""# FlowLoom Second Screen Display
*Session PID: {shell_pid}*

---

{content}

---

*Last updated: {timestamp}*"""
    
    with open(display_path, 'w') as f:
        f.write(display_content)
    
    print("✅ Display updated")
    print(f"📍 Path: {display_path}")
    return True

def main():
    parser = argparse.ArgumentParser(description='Update second screen display content')
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('content', nargs='?', help='Content to display')
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
    
    success = update_display(content)
    return 0 if success else 1

if __name__ == "__main__":
    sys.exit(main())