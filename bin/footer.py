#!/usr/bin/env python3
"""
FlowLoom Footer Formatter
Generates properly formatted interactive footer with ANSI box drawing characters.
"""

import argparse
import subprocess
import os
import textwrap
from datetime import datetime


def get_git_stats():
    """Get git status in symbolic format."""
    try:
        # Get modified files count
        result = subprocess.run(['git', 'diff', '--name-only'], 
                              capture_output=True, text=True, cwd=os.getcwd())
        modified = len(result.stdout.strip().split('\n')) if result.stdout.strip() else 0
        
        # Get untracked files count
        result = subprocess.run(['git', 'ls-files', '--others', '--exclude-standard'], 
                              capture_output=True, text=True, cwd=os.getcwd())
        untracked = len(result.stdout.strip().split('\n')) if result.stdout.strip() else 0
        
        if modified == 0 and untracked == 0:
            return ""
        
        stats = []
        if modified > 0:
            stats.append(f"{modified}M")
        if untracked > 0:
            stats.append(f"{untracked}?")
        
        return f" ({', '.join(stats)})"
        
    except subprocess.CalledProcessError:
        return ""


def get_current_branch():
    """Get current git branch."""
    try:
        result = subprocess.run(['git', 'branch', '--show-current'], 
                              capture_output=True, text=True, cwd=os.getcwd())
        return result.stdout.strip()
    except subprocess.CalledProcessError:
        return "unknown"


def get_working_dir():
    """Get current working directory."""
    return os.getcwd()


def check_memory_status():
    """Check status of dual memory systems."""
    import os
    import json
    from datetime import datetime, timedelta
    
    # Check fl-memory.json
    fl_memory_status = "❌"  # red x = not working
    try:
        fl_memory_path = "./.flowloom/fl-memory.json"
        if os.path.exists(fl_memory_path):
            with open(fl_memory_path, 'r') as f:
                # Check if file has recent activity (within last 5 minutes for interaction)
                stat = os.stat(fl_memory_path)
                modified_time = datetime.fromtimestamp(stat.st_mtime)
                if datetime.now() - modified_time < timedelta(minutes=5):
                    fl_memory_status = "✅"  # checkmark = used in this interaction
                else:
                    fl_memory_status = "⏸️"  # pause = healthy but unused in this interaction
    except:
        fl_memory_status = "❌"
    
    # Check basic-memory (doc system)
    doc_status = "❌"  # Default to not working
    try:
        knowledge_path = "./.flowloom/.knowledge"
        if os.path.exists(knowledge_path):
            # Check if knowledge directory has recent activity (within last 5 minutes for interaction)
            has_recent = False
            for root, dirs, files in os.walk(knowledge_path):
                for file in files:
                    file_path = os.path.join(root, file)
                    stat = os.stat(file_path)
                    modified_time = datetime.fromtimestamp(stat.st_mtime)
                    if datetime.now() - modified_time < timedelta(minutes=5):
                        has_recent = True
                        break
                if has_recent:
                    break
            
            if has_recent:
                doc_status = "✅"  # checkmark = used in this interaction
            else:
                doc_status = "⏸️"  # pause = healthy but unused in this interaction
    except:
        doc_status = "❌"
    
    return fl_memory_status, doc_status


def format_footer(dir_path, mode, branch, context, next_steps):
    """Format footer with ANSI box drawing characters in 3-row layout."""
    
    # Get memory system status
    mem_status, doc_status = check_memory_status()
    
    # Extract git stats from branch string
    git_stats = ""
    clean_branch = branch
    if " (" in branch and branch.endswith(")"):
        clean_branch, stats_part = branch.rsplit(" (", 1)
        git_stats = "(" + stats_part
    
    # Truncate long values for better display
    max_value_width = 28  # For regular columns
    
    # For directory, show end of path with leading ...
    if len(dir_path) > max_value_width:
        dir_short = "..." + dir_path[-(max_value_width-3):]
    else:
        dir_short = dir_path
    
    # Create 3-row layout: 
    # Row 1: Mem/Doc | Dir | Mode
    # Row 2: Memory indicators | Branch | Stats  
    # Row 3: Context (full width)
    rows = [
        ("Mem:", mem_status, "Dir:", dir_short, "Mode:", mode),
        ("Doc:", doc_status, "Branch:", clean_branch, "Stats:", git_stats)
    ]
    
    # Calculate column widths for 3-column layout
    left_col_width = 12   # Fixed width for left column (memory indicators)
    middle_col_width = 35 # Fixed width for middle column  
    right_col_width = 35  # Fixed width for right column
    total_width = left_col_width + middle_col_width + right_col_width + 4  # three columns + borders + separators
    
    # Build the 3-row table without ANSI box characters
    table_lines = []
    
    # First two rows with 3 columns
    for row in rows:
        left_label, left_value, middle_label, middle_value, right_label, right_value = row
        left_content = f"**{left_label}** {left_value}"
        middle_content = f"**{middle_label}** {middle_value}"
        right_content = f"**{right_label}** {right_value}"
        
        # Calculate padding for table alignment (account for markdown bold syntax)
        left_display_len = len(left_label) + len(str(left_value)) + 1  # label + space + value
        middle_display_len = len(middle_label) + len(str(middle_value)) + 1
        right_display_len = len(right_label) + len(str(right_value)) + 1
        
        left_padding = max(0, left_col_width - left_display_len)
        middle_padding = max(0, middle_col_width - middle_display_len)
        
        table_lines.append(f"{left_content}{' ' * left_padding}  {middle_content}{' ' * middle_padding}  {right_content}")
    
    # Third row: Context spanning full width
    context_content = f"**Context:** {context}"
    table_lines.append(f"{context_content}")
    
    # Format next steps
    steps_section = "### Next Steps"
    for step in next_steps.split('\\n'):
        if step.strip():
            steps_section += f"\n{step.strip()}"
    
    # Return table first, then next steps, then usage
    return '\n'.join(table_lines) + '\n\n' + steps_section + '\n\n💡 **Usage:** Type `go` to continue or select a numbered option above'


def has_git_changes():
    """Check if there are any modified or untracked files."""
    try:
        # Check for modified files
        result = subprocess.run(['git', 'diff', '--name-only'], 
                              capture_output=True, text=True, cwd=os.getcwd())
        modified = len(result.stdout.strip().split('\n')) if result.stdout.strip() else 0
        
        # Check for untracked files
        result = subprocess.run(['git', 'ls-files', '--others', '--exclude-standard'], 
                              capture_output=True, text=True, cwd=os.getcwd())
        untracked = len(result.stdout.strip().split('\n')) if result.stdout.strip() else 0
        
        return modified > 0 or untracked > 0
    except subprocess.CalledProcessError:
        return False


def main():
    parser = argparse.ArgumentParser(description='Generate FlowLoom interactive footer')
    parser.add_argument('--session', default='Development Session', 
                       help='Session context description')
    parser.add_argument('--next-steps', default='1. Continue with next task', 
                       help='Next steps (use \\n for line breaks)')
    parser.add_argument('--mode', default='None', 
                       help='Active mode (auto-detected if not provided)')
    parser.add_argument('--working-dir', 
                       help='Working directory (auto-detected if not provided)')
    parser.add_argument('--branch', 
                       help='Git branch (auto-detected if not provided)')
    
    args = parser.parse_args()
    
    # Auto-detect values if not provided
    working_dir = args.working_dir or get_working_dir()
    branch = args.branch or get_current_branch()
    git_stats = get_git_stats()
    branch_with_stats = f"{branch}{git_stats}"
    
    # Use provided mode or default
    mode = args.mode if args.mode != 'None' else 'Configuration Mode'
    
    # Default context based on session
    context = f"Dual memory system methodology established, {args.session.lower()}"
    
    # Auto-generate next steps if not provided and git changes detected
    if args.next_steps == '1. Continue with next task' and has_git_changes():
        next_steps = "1. Commit and push changes\\n2. Continue with next task"
    else:
        next_steps = args.next_steps
    
    # Generate and print footer
    footer = format_footer(working_dir, mode, branch_with_stats, context, next_steps)
    print(footer)


if __name__ == '__main__':
    main()