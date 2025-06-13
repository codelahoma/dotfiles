#!/usr/bin/env python
"""
Display Knowledge Base Content on Second Screen
Takes KB content as input and formats it with front matter as table and navigation.
"""

import os
import sys
import subprocess
import datetime
import argparse
import yaml
from pathlib import Path

def get_shell_pid():
    """Get the proper shell PID for display file naming."""
    try:
        result = subprocess.run(['./.flowloom/bin/get_shell_pid.py'], 
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

def parse_frontmatter_and_content(content):
    """Parse YAML frontmatter and content, return both parts."""
    if not content.startswith('---'):
        return None, content
    
    # Find the end of frontmatter
    lines = content.split('\n')
    frontmatter_end = -1
    for i, line in enumerate(lines[1:], 1):
        if line.strip() == '---':
            frontmatter_end = i
            break
    
    if frontmatter_end == -1:
        return None, content
    
    # Extract and parse frontmatter
    frontmatter_text = '\n'.join(lines[1:frontmatter_end])
    content_text = '\n'.join(lines[frontmatter_end + 1:])
    
    try:
        frontmatter_data = yaml.safe_load(frontmatter_text)
        return frontmatter_data, content_text
    except yaml.YAMLError:
        return None, content

def frontmatter_to_markdown_table(frontmatter_data):
    """Convert frontmatter dictionary to a markdown table."""
    if not frontmatter_data:
        return ""
    
    table = "## Metadata\n\n| Property | Value |\n|----------|-------|\n"
    
    for key, value in frontmatter_data.items():
        # Handle different types of values
        if isinstance(value, list):
            if all(isinstance(item, str) and item.startswith('#') for item in value):
                # Tags - remove # and join with commas
                formatted_value = ", ".join(tag.lstrip('#') for tag in value)
            else:
                formatted_value = ", ".join(str(item) for item in value)
        elif isinstance(value, dict):
            # Convert dict to key: value pairs
            formatted_value = ", ".join(f"{k}: {v}" for k, v in value.items())
        else:
            formatted_value = str(value)
        
        # Escape pipes in values
        formatted_value = formatted_value.replace('|', '\\|')
        
        table += f"| {key} | {formatted_value} |\n"
    
    return table + "\n"

def add_navigation_links(archive_name=None, shell_pid=None):
    """Add simple navigation links in table format."""
    nav_section = ""
    
    # Get shell PID if not provided
    if not shell_pid:
        shell_pid = get_shell_pid()
    
    # Create navigation table with session info - use onclick to avoid browser history
    if archive_name:
        prev_link = f"<a href=\"./history/{archive_name}\" onclick=\"window.location.replace(this.href); return false;\">◀</a>"
    else:
        prev_link = "◀"
    next_link = "▶"  # Will be updated when next display exists
    
    nav_section += f"| {prev_link} | Session: {shell_pid} | {next_link} |\n"
    nav_section += "|---|---|---|\n\n"
    
    return nav_section

def find_most_recent_archive():
    """Find the most recent archived display for this session."""
    shell_pid = get_shell_pid()
    history_dir = Path.cwd() / '.flowloom' / 'displays' / 'history'
    
    if not history_dir.exists():
        return None
    
    # Find all archived displays for this session
    archive_pattern = f"archived_*_claude_display_{shell_pid}.md"
    archives = list(history_dir.glob(archive_pattern))
    
    if not archives:
        return None
    
    # Sort by modification time to get most recent
    archives.sort(key=lambda p: p.stat().st_mtime, reverse=True)
    return archives[0]

def update_archive_forward_link(archive_path, new_target):
    """Update the forward link in an archived display."""
    if not archive_path.exists():
        return False
    
    with open(archive_path, 'r') as f:
        content = f.read()
    
    # Find and replace the forward link in the navigation table
    lines = content.split('\n')
    for i, line in enumerate(lines):
        if '|' in line and 'Session:' in line and '|' in line:
            # This is likely our navigation line
            parts = line.split('|')
            if len(parts) >= 4:
                # Update the forward link (last column before final |)
                parts[-2] = f" <a href=\"{new_target}\" onclick=\"window.location.replace(this.href); return false;\">▶</a> "
                lines[i] = '|'.join(parts)
                break
    
    # Write back the updated content
    with open(archive_path, 'w') as f:
        f.write('\n'.join(lines))
    
    return True

def update_navigation_in_content(content, prev_link, next_link, shell_pid):
    """Update the navigation links in existing content."""
    lines = content.split('\n')
    nav_line = f"| {prev_link} | Session: {shell_pid} | {next_link} |"
    
    # Find and replace the first navigation line
    for i, line in enumerate(lines):
        if '|' in line and 'Session:' in line and '|' in line:
            lines[i] = nav_line
            break
    
    return '\n'.join(lines)

def archive_current_display():
    """Archive the current display to history."""
    current_path, archive_path, archive_name = get_display_paths()
    
    if not current_path.exists():
        return None
    
    # Find the most recent archive to update its forward link
    most_recent_archive = find_most_recent_archive()
    
    # Read current content
    with open(current_path, 'r') as f:
        content = f.read()
    
    # Get shell PID for display file name
    shell_pid = get_shell_pid()
    display_file = f"claude_display_{shell_pid}.md"
    
    # Update navigation in the content for archived version
    if most_recent_archive:
        # Link to previous archive (same directory)
        prev_archive_name = most_recent_archive.name
        prev_link = f"<a href=\"{prev_archive_name}\" onclick=\"window.location.replace(this.href); return false;\">◀</a>"
    else:
        # No previous archive
        prev_link = "◀"
    
    next_link = f"<a href=\"../{display_file}\" onclick=\"window.location.replace(this.href); return false;\">▶</a>"  # Link back to current display
    
    # Update the navigation in the existing content
    enhanced_content = update_navigation_in_content(content, prev_link, next_link, shell_pid)
    
    # Write to archive
    with open(archive_path, 'w') as f:
        f.write(enhanced_content)
    
    # Update the previous archive's forward link to point to this new archive
    if most_recent_archive:
        update_archive_forward_link(most_recent_archive, archive_name)
    
    return archive_name

def is_welcome_screen(content):
    """Check if the current display is a welcome screen that shouldn't be archived."""
    return ("This is your second screen for assessments, plans, and other conversation artifacts" in content or
            "Display ready for content updates" in content)

def display_kb_content(kb_content, title="Knowledge Base Content"):
    """Main function to display KB content on second screen."""
    current_path, _, _ = get_display_paths()
    
    if not current_path.exists():
        print("❌ No active second screen found")
        print("💡 Create one with: ./.flowloom/bin/second-screen")
        return False
    
    # Check if current display is a welcome screen
    archive_name = None
    with open(current_path, 'r') as f:
        current_content = f.read()
    
    if not is_welcome_screen(current_content):
        # Archive current display only if it's not a welcome screen
        archive_name = archive_current_display()
        if archive_name:
            print(f"✅ Display archived as: {archive_name}")
    else:
        print("ℹ️ Welcome screen replaced (not archived)")
    
    # Parse frontmatter and content
    frontmatter_data, main_content = parse_frontmatter_and_content(kb_content)
    
    # Convert frontmatter to markdown table
    metadata_table = frontmatter_to_markdown_table(frontmatter_data)
    
    # Create navigation breadcrumb
    nav_section = add_navigation_links(archive_name)
    
    # Format the complete display content
    shell_pid = get_shell_pid()
    timestamp = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    
    display_content = f"""{nav_section}---

{metadata_table}{main_content}

---

*Last updated: {timestamp}*"""
    
    # Write to current display
    with open(current_path, 'w') as f:
        f.write(display_content)
    
    print(f"✅ KB content displayed on second screen")
    print(f"📍 Path: {current_path}")
    return True

def main():
    parser = argparse.ArgumentParser(description='Display knowledge base content on second screen')
    parser.add_argument('--title', default='Knowledge Base Content', help='Title for the content')
    
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('--content', help='KB content as string')
    group.add_argument('--from-file', '-f', help='Read KB content from file')
    
    args = parser.parse_args()
    
    if args.from_file:
        try:
            with open(args.from_file, 'r') as f:
                kb_content = f.read()
        except FileNotFoundError:
            print(f"❌ File not found: {args.from_file}")
            return 1
        except Exception as e:
            print(f"❌ Error reading file: {e}")
            return 1
    else:
        kb_content = args.content
    
    success = display_kb_content(kb_content, args.title)
    return 0 if success else 1

if __name__ == "__main__":
    sys.exit(main())