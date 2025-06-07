#!/bin/bash

# FlowLoom Footer Formatting Script
# Generates consistent interactive footer with proper formatting

set -euo pipefail

# Default values
working_dir=""
active_mode=""
session_context=""
session_id=""
git_branch=""
next_steps=""
claude_model=""

# Function to show usage
show_usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Options:
  -w, --working-dir DIR     Set working directory (default: current pwd)
  -m, --mode MODE          Set active mode (default: detect or "None")
  -s, --session CONTEXT   Set session context (required)
  -b, --branch BRANCH     Set git branch (default: auto-detect)
  -i, --session-id ID     Set session ID (default: auto-detect)
  -n, --next-steps STEPS  Set next steps (required)
  -h, --help              Show this help

Examples:
  $0 -s "Working on footer script" -n "1. Test script\\n2. Commit changes"
  $0 --session "Bug fixing" --next-steps "Fix remaining issues"
  
Next steps format:
  - Single step: "Complete the implementation"
  - Multiple steps: "1. First step\\n2. Second step\\n3. Third step"
EOF
}

# Function to detect git branch
detect_git_branch() {
    if git rev-parse --git-dir >/dev/null 2>&1; then
        git branch --show-current 2>/dev/null || echo "detached"
    else
        echo "no-git"
    fi
}

# Function to detect current session ID
detect_current_session() {
    local script_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
    local session_script="$script_dir/get_current_session.sh"
    
    if [[ -x "$session_script" ]]; then
        "$session_script" 2>/dev/null || echo "no-session"
    else
        echo "no-session"
    fi
}

# Function to detect active FlowLoom mode
detect_active_mode() {
    # Check for mode indicators in environment or recent commands
    # For now, default to "None" - could be enhanced to detect actual modes
    echo "None"
}

# Function to detect active Claude model
detect_claude_model() {
    # Note: Claude can directly provide model information rather than parsing shell output
    # The shell command approach may not always work reliably
    # Better to have Claude directly state the model in use
    echo "ask-claude-directly"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -w|--working-dir)
            working_dir="$2"
            shift 2
            ;;
        -m|--mode)
            active_mode="$2"
            shift 2
            ;;
        -s|--session)
            session_context="$2"
            shift 2
            ;;
        -b|--branch)
            git_branch="$2"
            shift 2
            ;;
        -i|--session-id)
            session_id="$2"
            shift 2
            ;;
        -n|--next-steps)
            next_steps="$2"
            shift 2
            ;;
        --model)
            claude_model="$2"
            shift 2
            ;;
        -h|--help)
            show_usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            show_usage >&2
            exit 1
            ;;
    esac
done

# Validate required parameters
if [[ -z "$session_context" ]]; then
    echo "Error: Session context is required (-s/--session)" >&2
    show_usage >&2
    exit 1
fi

if [[ -z "$next_steps" ]]; then
    echo "Error: Next steps are required (-n/--next-steps)" >&2
    show_usage >&2
    exit 1
fi

# Set defaults for optional parameters
if [[ -z "$working_dir" ]]; then
    working_dir=$(pwd)
fi

if [[ -z "$active_mode" ]]; then
    active_mode=$(detect_active_mode)
fi

if [[ -z "$session_id" ]]; then
    session_id=$(detect_current_session)
fi

if [[ -z "$git_branch" ]]; then
    git_branch=$(detect_git_branch)
fi

if [[ -z "$claude_model" ]]; then
    claude_model=$(detect_claude_model)
fi

# Format next steps
formatted_next_steps=""
if [[ "$next_steps" == *$'\n'* ]] || [[ "$next_steps" == *"\\n"* ]]; then
    # Multiple steps - already numbered or need numbering
    if [[ "$next_steps" != *"1."* ]]; then
        # Auto-number steps
        step_num=1
        while IFS= read -r line; do
            if [[ -n "$line" ]]; then
                formatted_next_steps+="${step_num}. ${line}"$'\n'
                ((step_num++))
            fi
        done <<< "${next_steps//\\n/$'\n'}"
        formatted_next_steps=${formatted_next_steps%$'\n'}  # Remove trailing newline
    else
        # Already numbered
        formatted_next_steps="${next_steps//\\n/$'\n'}"
    fi
    next_steps_label="**Next steps (type 'go' for all, or numbers like '1 3'):**"
else
    # Single step
    formatted_next_steps="$next_steps"
    next_steps_label="**Next steps:**"
fi

# Capture to memory before generating footer
SHELL_PID=$(./bin/get_shell_pid.sh 2>/dev/null || echo "unknown")
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
MEMORY_OBSERVATION="Shell_ID: $SHELL_PID - FOOTER: $TIMESTAMP | Context: $session_context | Branch: $git_branch | Mode: $active_mode | Next: $formatted_next_steps"
SESSION_ENTITY="Session-$SHELL_PID-footer-$(date +%s)"

# Add to memory using log_observation
if python3 "$(dirname "$0")/log_observation.py" --memory-file fl-memory.json add-entity "$SESSION_ENTITY" "Session" "$MEMORY_OBSERVATION" >/dev/null 2>&1; then
    memory_status="✅ Memory captured"
else
    memory_status="⚠️ Memory capture failed"
fi

# Generate the footer
cat << EOF
---

**Working directory:** $working_dir
**Active mode:** $active_mode
**Session context:** $session_context
**Session ID:** $session_id
**Git branch:** $git_branch
**Claude model:** $claude_model
**Memory status:** $memory_status
$next_steps_label
$formatted_next_steps

<!-- CLAUDE: Remember to format your response with newlines between each footer item (no blank lines) per CLAUDE.local.md guidelines -->
EOF