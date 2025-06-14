#!/usr/bin/env python3
"""
Archive Current Second Screen Display
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

def get_display_paths():
    """Get the paths for current and archived displays."""
    shell_pid = get_shell_pid()
    base_dir = Path.cwd() / '.flowloom' / 'displays'
    history_dir = base_dir / 'history'
    
    # Ensure directories exist
    history_dir.mkdir(parents=True, exist_ok=True)
    
    display_file = f"claude_display_{shell_pid}.md"
    current_path = base_dir / display_file
    
    timestamp = datetime.datetime.now().strftime('%Y%m%d_%H%M%S')
    archive_name = f"archived_{timestamp}_{display_file}"
    archive_path = history_dir / archive_name
    
    return current_path, archive_path, archive_name

def add_navigation_links(content, archive_name=None, next_archive=None):
    """Add navigation links to archived content."""
    nav_section = "\n---\n\n## Navigation\n"
    
    if archive_name:
        nav_section += f"- **Previous**: [📄 Previous Display](./history/{archive_name})\n"
    else:
        nav_section += "- **Previous**: [No previous display]\n"
    
    if next_archive:
        nav_section += f"- **Next**: [📄 Next Display](./history/{next_archive})\n"
    else:
        nav_section += "- **Next**: [No next display yet]\n"
    
    nav_section += "\n---\n"
    
    # Insert navigation before the last updated line
    lines = content.split('\n')
    if lines and lines[-1].startswith('*Last updated:'):
        lines.insert(-1, nav_section.strip())
        return '\n'.join(lines)
    else:
        return content + nav_section

def archive_display(create_new=False):
    """Archive the current display."""
    current_path, archive_path, archive_name = get_display_paths()
    
    if not current_path.exists():
        print(f"❌ No active display found for PID {get_shell_pid()}")
        print("💡 Create one with: ./.flowloom/bin/second-screen")
        return False, None
    
    # Read current content
    with open(current_path, 'r') as f:
        content = f.read()
    
    # Add navigation links to the archived content
    enhanced_content = add_navigation_links(content)
    
    # Write to archive
    with open(archive_path, 'w') as f:
        f.write(enhanced_content)
    
    # Remove current display
    current_path.unlink()
    
    print(f"✅ Display archived as: {archive_name}")
    print("🗑️ Current display cleared")
    
    if create_new:
        print("\n🔄 Creating new display...")
        subprocess.run(['./.flowloom/bin/second-screen'])
    else:
        print("\n💡 To create a new display: ./.flowloom/bin/second-screen")
    
    return True, archive_name

def main():
    parser = argparse.ArgumentParser(description='Archive current second screen display')
    parser.add_argument('--new', action='store_true', 
                       help='Create new display after archiving')
    
    args = parser.parse_args()
    
    success, archive_name = archive_display(create_new=args.new)
    return 0 if success else 1

if __name__ == "__main__":
    sys.exit(main())