#!/usr/bin/env python3
"""
Show Knowledge Base Content on Second Screen
Handles archiving current display, copying KB content, and adding navigation breadcrumbs.
"""

import os
import sys
import subprocess
import datetime
import argparse
import re
import yaml
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

def add_navigation_links(content, archive_name=None):
    """Add navigation links to archived content."""
    nav_section = "## Navigation\n"
    
    if archive_name:
        nav_section += f"- **Previous**: [📄 Previous Display](./history/{archive_name})\n"
    else:
        nav_section += "- **Previous**: [No previous display]\n"
    
    nav_section += "- **Next**: [No next display yet]\n\n"
    
    return nav_section

def archive_current_display():
    """Archive the current display to history."""
    current_path, archive_path, archive_name = get_display_paths()
    
    if not current_path.exists():
        return None
    
    # Read current content
    with open(current_path, 'r') as f:
        content = f.read()
    
    # Add navigation links to the archived content
    nav_section = "\n---\n\n" + add_navigation_links(content) + "---\n"
    
    # Insert navigation before the last updated line
    lines = content.split('\n')
    if lines and lines[-1].startswith('*Last updated:'):
        lines.insert(-1, nav_section.strip())
        enhanced_content = '\n'.join(lines)
    else:
        enhanced_content = content + nav_section
    
    # Write to archive
    with open(archive_path, 'w') as f:
        f.write(enhanced_content)
    
    print(f"✅ Display archived as: {archive_name}")
    return archive_name

def query_knowledge_base(content_identifier):
    """Query the knowledge base for content - placeholder for now."""
    # This will be called from Claude Code context where MCP tools are available
    # For now, return a placeholder that indicates the KB query should happen in the calling context
    return f"KB_QUERY_PLACEHOLDER:{content_identifier}"

def show_kb_content(content_identifier):
    """Main function to show KB content on second screen."""
    current_path, _, _ = get_display_paths()
    
    if not current_path.exists():
        print("❌ No active second screen found")
        print("💡 Create one with: ./.flowloom/bin/second-screen")
        return False
    
    # Archive current display
    archive_name = archive_current_display()
    
    # Query knowledge base
    kb_content = query_knowledge_base(content_identifier)
    
    # Parse frontmatter and content
    frontmatter_data, main_content = parse_frontmatter_and_content(kb_content)
    
    # Convert frontmatter to markdown table
    metadata_table = frontmatter_to_markdown_table(frontmatter_data)
    
    # Create navigation breadcrumb
    nav_section = add_navigation_links("", archive_name)
    
    # Format the complete display content
    shell_pid = get_shell_pid()
    timestamp = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    
    display_content = f"""# FlowLoom Second Screen Display
*Session PID: {shell_pid}*

---

{nav_section}---

{metadata_table}{main_content}

---

*Last updated: {timestamp}*"""
    
    # Write to current display
    with open(current_path, 'w') as f:
        f.write(display_content)
    
    print(f"✅ KB content '{content_identifier}' displayed on second screen")
    print(f"📍 Path: {current_path}")
    return True

def main():
    parser = argparse.ArgumentParser(description='Show knowledge base content on second screen')
    parser.add_argument('content_identifier', help='KB content identifier (note ID, title, or search term)')
    
    args = parser.parse_args()
    
    success = show_kb_content(args.content_identifier)
    return 0 if success else 1

if __name__ == "__main__":
    sys.exit(main())