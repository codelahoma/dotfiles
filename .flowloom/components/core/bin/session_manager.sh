#!/bin/bash
# bin/session_manager.sh - Lightweight wrapper for FlowLoom Python session management
# Delegates to packages/flowloom_session for actual implementation
#
# SECURITY & ETHICS NOTICE:
# This script provides a simple interface to the Python session management system
# while respecting Claude Code's security boundaries.

set -euo pipefail

# Get script directory for path resolution
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Python package path
PYTHON_PKG_PATH="$PROJECT_ROOT/packages"

# Default configuration
readonly DEFAULT_BASE_DIR="${FLOWLOOM_WORK_DIR:-.flowloom}"

# Standardized shell PID detection function
get_shell_pid() {
    local script_path="$SCRIPT_DIR/get_shell_pid.sh"
    
    if [[ -f "$script_path" ]]; then
        local pid_result
        pid_result=$("$script_path" 2>/dev/null)
        if [[ -n "$pid_result" && "$pid_result" != "0" ]]; then
            echo "$pid_result"
            return 0
        fi
    fi
    
    # Fallback to PPID
    echo "${PPID:-$$}"
}

# Call Python session management with all arguments
call_python_session_manager() {
    local shell_pid=$(get_shell_pid)
    
    # Set up Python path and environment
    export PYTHONPATH="$PYTHON_PKG_PATH:${PYTHONPATH:-}"
    export FLOWLOOM_SHELL_PID="$shell_pid"
    export FLOWLOOM_WORK_DIR="${FLOWLOOM_WORK_DIR:-$DEFAULT_BASE_DIR}"
    
    # Call the Python session manager
    python3 -m flowloom_session.cli "$@"
}

# Print help for the wrapper
print_help() {
    cat << EOF
FlowLoom Session Manager (Bash Wrapper)

Usage: session_manager.sh <command> [options]

This is a lightweight wrapper around the Python session management system.
All commands are delegated to the Python implementation for main-only workflow.

Commands:
  start [description]     Start new session in main branch
  status [detailed]       Show current session status  
  stop [note]            Stop current session
  list [filter]          List sessions
  backup [type]          Create session backup
  recover [session_id]   Recover previous session
  help                   Show this help

Examples:
  session_manager.sh start "Working on session management fixes"
  session_manager.sh status
  session_manager.sh stop "Completed implementation"
  session_manager.sh list active

Note: This wrapper no longer uses git worktrees. All sessions run in main branch.
EOF
}

# Main command dispatcher
main() {
    if [[ $# -eq 0 ]]; then
        print_help
        exit 1
    fi
    
    local command="$1"
    
    case "$command" in
        "help"|"--help"|"-h")
            print_help
            ;;
        *)
            # Delegate all other commands to Python implementation
            call_python_session_manager "$@"
            ;;
    esac
}

# Run main function if script is executed directly
if [[ "${BASH_SOURCE[0]:-$0}" == "${0}" ]]; then
    main "$@"
fi