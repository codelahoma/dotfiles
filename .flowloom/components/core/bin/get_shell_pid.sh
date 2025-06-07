#!/bin/bash
# bin/get_shell_pid.sh - Centralized shell PID detection for FlowLoom
# Provides consistent shell ID across all FlowLoom components
#
# SECURITY & ETHICS NOTICE:
# This script provides standardized shell detection while respecting Claude Code's
# security boundaries. It uses only standard process inspection tools.

set -euo pipefail

# Show security warning before shell PID detection
show_security_warning() {
    cat << 'EOF' >&2
⚠️  SECURITY & PERMISSION NOTICE
================================

This script needs to detect your shell process ID (PID) for FlowLoom coordination.

WHY THIS IS NEEDED:
- Enables multi-Claude session coordination
- Provides unique identifiers for worker/controller roles
- Allows proper session tracking and memory correlation

HOW WE RESPECT CLAUDE'S SECURITY MODEL:
- Uses only standard process inspection tools (ps, pstree)
- No privileged access or system modification
- Read-only process information only
- Transparent detection methods documented below

DETECTION METHODS:
1. Environment variables (CLAUDE_PPID if set)
2. Process tree walk to find shell that launched Claude
3. Standard pstree analysis (if available)
4. PPID fallback for subprocess detection

Your consent is required each time Claude restarts as we cannot cache permission decisions.

Proceed with shell PID detection? The process will be transparent and logged.
EOF
}

# Standardized shell PID detection function
get_shell_pid() {
    # Show security warning for interactive sessions
    if [[ -t 0 && -t 2 ]] && [[ "${SKIP_SECURITY_WARNING:-}" != "true" ]]; then
        show_security_warning
        echo -n "Continue? (y/N): " >&2
        read -r consent
        if [[ "$consent" != "y" && "$consent" != "Y" ]]; then
            echo "Shell PID detection cancelled by user." >&2
            exit 1
        fi
        echo "Proceeding with shell PID detection..." >&2
        echo "" >&2
    fi
    # Try multiple methods in order of preference (with transparency)
    if [[ -n "${CLAUDE_PPID:-}" ]]; then
        echo "$CLAUDE_PPID"
        return 0
    fi
    
    # Method 1: Walk up process tree to find shell that launched Claude
    # Start from current process and walk up the tree
    local current_pid=$$
    local parent_pid
    
    # Walk up more steps to handle nested contexts (session manager, etc.)
    for i in {1..10}; do
        parent_pid=$(ps -o ppid= -p "$current_pid" 2>/dev/null | tr -d ' ')
        if [[ -z "$parent_pid" || "$parent_pid" == "1" ]]; then
            break
        fi
        
        # Check if this is Claude (node process)
        local process_name
        process_name=$(ps -o comm= -p "$parent_pid" 2>/dev/null)
        if [[ "$process_name" == "node" ]]; then
            # Found Claude, now get its parent (the shell)
            local claude_parent
            claude_parent=$(ps -o ppid= -p "$parent_pid" 2>/dev/null | tr -d ' ')
            if [[ -n "$claude_parent" && "$claude_parent" != "1" ]]; then
                echo "$claude_parent"
                return 0
            fi
        fi
        
        current_pid="$parent_pid"
    done
    
    # Method 2: Use pstree to find shell that launched Claude (if available)
    if command -v pstree >/dev/null 2>&1; then
        local shell_pid
        shell_pid=$(pstree -p $$ 2>/dev/null | grep -o 'zsh([0-9]*)' | head -1 | grep -o '[0-9]*' 2>/dev/null)
        if [[ -n "$shell_pid" && "$shell_pid" != "$$" ]]; then
            echo "$shell_pid"
            return 0
        fi
        # Try bash as well
        shell_pid=$(pstree -p $$ 2>/dev/null | grep -o 'bash([0-9]*)' | head -1 | grep -o '[0-9]*' 2>/dev/null)
        if [[ -n "$shell_pid" && "$shell_pid" != "$$" ]]; then
            echo "$shell_pid"
            return 0
        fi
    fi
    
    # Method 3: Fallback to PPID (may be subprocess but better than nothing)
    if [[ -n "${PPID:-}" ]]; then
        echo "$PPID"
        return 0
    fi
    
    # Method 4: Last resort - current process
    echo "$$"
}

# Print help information
print_help() {
    cat << 'EOF'
get_shell_pid.sh - Centralized shell PID detection for FlowLoom

USAGE:
  ./bin/get_shell_pid.sh                    Output shell PID
  ./bin/get_shell_pid.sh --help|-h          Show this help
  ./bin/get_shell_pid.sh --debug            Show detection process
  ./bin/get_shell_pid.sh --export           Export SHELL_PID variable

METHODS:
  1. CLAUDE_PPID environment variable (if set)
  2. Process tree walk to find shell that launched Claude
  3. pstree analysis (if available)
  4. PPID fallback
  5. Current process (last resort)

INTEGRATION:
  # From shell scripts:
  shell_pid=$(./bin/get_shell_pid.sh)
  
  # Export for environment:
  eval "$(./bin/get_shell_pid.sh --export)"
  
  # From other scripts:
  source bin/get_shell_pid.sh
  shell_pid=$(get_shell_pid)

EXAMPLES:
  # Session management:
  session_id="$(./bin/get_shell_pid.sh)-feature"
  
  # Memory tagging:
  shell_pid=$(./bin/get_shell_pid.sh)
  observation="Shell_ID: $shell_pid"
  
  # Claude commands:
  SHELL_PID=$(./bin/get_shell_pid.sh)

This ensures consistent shell identification across all FlowLoom components.
EOF
}

# Debug mode - show detection process
debug_detection() {
    echo "=== Shell PID Detection Debug ===" >&2
    echo "Current process: $$" >&2
    
    if [[ -n "${CLAUDE_PPID:-}" ]]; then
        echo "Method 1: CLAUDE_PPID environment variable found: $CLAUDE_PPID" >&2
        echo "$CLAUDE_PPID"
        return 0
    fi
    
    echo "Method 1: CLAUDE_PPID not set, walking process tree..." >&2
    local current_pid=$$
    local parent_pid
    
    for i in {1..10}; do
        parent_pid=$(ps -o ppid= -p "$current_pid" 2>/dev/null | tr -d ' ')
        echo "  Step $i: Process $current_pid -> Parent $parent_pid" >&2
        
        if [[ -z "$parent_pid" || "$parent_pid" == "1" ]]; then
            echo "  Reached init or empty parent, stopping" >&2
            break
        fi
        
        local process_name
        process_name=$(ps -o comm= -p "$parent_pid" 2>/dev/null)
        echo "  Parent process: $parent_pid ($process_name)" >&2
        
        if [[ "$process_name" == "node" ]]; then
            echo "  Found Claude (node), getting its parent..." >&2
            local claude_parent
            claude_parent=$(ps -o ppid= -p "$parent_pid" 2>/dev/null | tr -d ' ')
            echo "  Claude parent (shell): $claude_parent" >&2
            if [[ -n "$claude_parent" && "$claude_parent" != "1" ]]; then
                echo "Method 1 SUCCESS: Shell PID $claude_parent" >&2
                echo "$claude_parent"
                return 0
            fi
        fi
        
        current_pid="$parent_pid"
    done
    
    echo "Method 1 failed, trying pstree..." >&2
    if command -v pstree >/dev/null 2>&1; then
        echo "Method 2: pstree available" >&2
        get_shell_pid
    else
        echo "Method 2: pstree not available" >&2
        if [[ -n "${PPID:-}" ]]; then
            echo "Method 3: Using PPID fallback: $PPID" >&2
            echo "$PPID"
        else
            echo "Method 4: Last resort - current process: $$" >&2
            echo "$$"
        fi
    fi
}

# Main execution
main() {
    case "${1:-}" in
        --help|-h)
            print_help
            ;;
        --debug)
            debug_detection
            ;;
        --export)
            local shell_pid
            shell_pid=$(get_shell_pid)
            echo "export SHELL_PID=$shell_pid"
            ;;
        "")
            get_shell_pid
            ;;
        *)
            echo "Error: Unknown option '$1'" >&2
            echo "Use --help for usage information" >&2
            exit 1
            ;;
    esac
}

# Only run main if script is executed directly (not sourced)
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi