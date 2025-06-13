#!/usr/bin/env python3
"""
Replace Section in Second Screen Display
"""

import os
import sys
import subprocess
import datetime
import argparse
import re
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

def replace_section(section_header, new_content):
    """Replace a specific section in the display."""
    display_path = get_display_path()
    
    if not display_path.exists():
        print(f"❌ No active display found for PID {get_shell_pid()}")
        print("💡 Create one with: ./.flowloom/bin/second-screen")
        return False
    
    # Read current content
    with open(display_path, 'r') as f:
        content = f.read()
    
    lines = content.split('\n')
    new_lines = []
    in_target_section = False
    section_found = False
    
    for line in lines:
        if line.startswith('##') and section_header.lower() in line.lower():
            # Found the target section
            new_lines.append(line)
            new_lines.append('')
            new_lines.append(new_content)
            new_lines.append('')
            in_target_section = True
            section_found = True
        elif line.startswith('##') and in_target_section:
            # End of target section, start of new section
            in_target_section = False
            new_lines.append(line)
        elif not in_target_section:
            # Not in target section, keep line
            new_lines.append(line)
        # Skip lines that are in target section (they get replaced)
    
    # If section wasn't found, add it
    if not section_found:
        # Insert before the last updated line
        if new_lines and new_lines[-1].startswith('*Last updated:'):
            new_lines.insert(-1, '')
            new_lines.insert(-1, f"## {section_header}")
            new_lines.insert(-1, '')
            new_lines.insert(-1, new_content)
            new_lines.insert(-1, '')
        else:
            new_lines.extend(['', f"## {section_header}", '', new_content, ''])
    
    # Update timestamp
    timestamp = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    if new_lines and new_lines[-1].startswith('*Last updated:'):
        new_lines[-1] = f'*Last updated: {timestamp}*'
    else:
        new_lines.append(f'*Last updated: {timestamp}*')
    
    # Write back to file
    with open(display_path, 'w') as f:
        f.write('\n'.join(new_lines))
    
    action = "updated" if section_found else "added"
    print(f"✅ Section '{section_header}' {action} in display")
    print(f"📍 Path: {display_path}")
    return True

def main():
    parser = argparse.ArgumentParser(description='Replace section in second screen display')
    parser.add_argument('section_header', help='Section header to find and replace')
    
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('new_content', nargs='?', help='New content for the section')
    group.add_argument('--from-file', '-f', help='Read new content from file')
    
    args = parser.parse_args()
    
    if args.from_file:
        try:
            with open(args.from_file, 'r') as f:
                new_content = f.read().strip()
        except FileNotFoundError:
            print(f"❌ File not found: {args.from_file}")
            return 1
        except Exception as e:
            print(f"❌ Error reading file: {e}")
            return 1
    else:
        new_content = args.new_content
    
    success = replace_section(args.section_header, new_content)
    return 0 if success else 1

if __name__ == "__main__":
    sys.exit(main())