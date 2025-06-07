#!/bin/bash
# bin/get_current_session.sh - Get current active session for the shell
# Part of FlowLoom Session Management System
#
# SECURITY & ETHICS NOTICE:
# This script retrieves the current active session while respecting Claude Code's
# security boundaries. It only accesses session data within approved workflows.

set -euo pipefail

# Get script directory for path resolution
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Get current shell PID
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

# Get current active session (most recent session for this shell)
get_current_session() {
    local output_format="${1:-session_id}"  # session_id, purpose, json, details
    local shell_pid
    shell_pid=$(get_shell_pid)
    
    # Set up Python path and environment
    export PYTHONPATH="$PROJECT_ROOT/packages:${PYTHONPATH:-}"
    export FLOWLOOM_SHELL_PID="$shell_pid"
    
    python3 -c "
from flowloom_session import quick_list_shell_sessions
import json
import sys

try:
    shell_pid = $shell_pid
    result = quick_list_shell_sessions(shell_pid)
    
    if not result['sessions']:
        if '$output_format' == 'json':
            print(json.dumps({'error': 'No active sessions found', 'shell_pid': shell_pid}))
        else:
            print('', end='')  # Empty output for no sessions
        sys.exit(1)
    
    # Find the most recent session (by created timestamp)
    latest_session = max(result['sessions'], key=lambda s: s['created'])
    
    if '$output_format' == 'session_id':
        print(latest_session['session_id'])
    elif '$output_format' == 'purpose':
        print(latest_session['purpose'])
    elif '$output_format' == 'purpose_id':
        print(latest_session['purpose_id'])
    elif '$output_format' == 'raw_purpose':
        print(latest_session['raw_purpose'])
    elif '$output_format' == 'details':
        print(f\"Session: {latest_session['session_id']}\")
        print(f\"Purpose: {latest_session['purpose']}\")
        print(f\"Status: {latest_session['status']}\")
        print(f\"Created: {latest_session['created']}\")
    elif '$output_format' == 'json':
        print(json.dumps(latest_session, indent=2))
    else:
        print(latest_session['session_id'])
        
except Exception as e:
    if '$output_format' == 'json':
        print(json.dumps({'error': str(e), 'shell_pid': shell_pid}))
    else:
        print(f'Error: {e}', file=sys.stderr)
    sys.exit(1)
"
}

# Print help
print_help() {
    cat << EOF
get_current_session.sh - Get current active session for the shell

USAGE:
  ./bin/get_current_session.sh [format]

FORMATS:
  session_id    - Session ID only (default)
  purpose       - Session purpose/description
  purpose_id    - Short purpose identifier
  raw_purpose   - Original purpose text
  details       - Multi-line session details
  json          - Full session data as JSON

EXAMPLES:
  # Get current session ID
  current_session=\$(./bin/get_current_session.sh)
  
  # Get session purpose for display
  purpose=\$(./bin/get_current_session.sh purpose)
  
  # Get full details
  ./bin/get_current_session.sh details
  
  # Get JSON for parsing
  ./bin/get_current_session.sh json | jq '.purpose'

INTEGRATION:
  # In footer formatting:
  session_id=\$(./bin/get_current_session.sh)
  purpose=\$(./bin/get_current_session.sh purpose)
  
  # Shell prompt integration:
  FLOWLOOM_SESSION=\$(./bin/get_current_session.sh purpose_id)

EXIT CODES:
  0 - Success, session found
  1 - No active sessions or error

This provides the current active session (most recent) for the current shell.
EOF
}

# Main execution
main() {
    case "${1:-}" in
        --help|-h|help)
            print_help
            ;;
        session_id|purpose|purpose_id|raw_purpose|details|json|"")
            get_current_session "${1:-session_id}"
            ;;
        *)
            echo "Error: Unknown format '$1'" >&2
            echo "Use --help for usage information" >&2
            exit 1
            ;;
    esac
}

# Run main function if script is executed directly
if [[ "${BASH_SOURCE[0]:-$0}" == "${0}" ]]; then
    main "$@"
fi