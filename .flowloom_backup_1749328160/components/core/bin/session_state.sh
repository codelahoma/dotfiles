#!/bin/bash
# bin/session_state.sh - Session state persistence and versioning with JSON
# Part of FlowLoom Session Management System - Phase 2: State Management
# Based on Plan 3120 - Session Tracking Detailed Design
#
# SECURITY & ETHICS NOTICE:
# This script manages session state persistence while respecting Claude Code's
# security boundaries. All operations are confined to designated session directories
# and do not access unauthorized resources or attempt to circumvent security measures.

set -euo pipefail

# Import session utilities
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
source "$SCRIPT_DIR/session_utils.sh"
source "$SCRIPT_DIR/session_filesystem.sh"

# State management configuration
readonly STATE_VERSION=1
readonly MAX_STATE_BACKUPS=5
readonly STATE_FILE_NAME="state.json"

# Session state management functions

# Get default state structure
get_default_session_state() {
    cat << 'EOF'
{
  "version": 1,
  "timestamp": "",
  "sessionId": "",
  "data": {
    "currentPlan": null,
    "activeCommands": [],
    "recentFiles": [],
    "gitState": {
      "branch": "main",
      "uncommittedFiles": [],
      "lastCommit": null
    },
    "modeHistory": [],
    "contextHistory": [],
    "artifacts": [],
    "todoItems": [],
    "memoryEntities": []
  }
}
EOF
}

# Initialize state file for a session
initialize_session_state() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    
    session_debug "Initializing state for session: $session_id"
    
    if ! validate_session_id "$session_id"; then
        session_error "Invalid session ID: $session_id"
        return 1
    fi
    
    local session_dir
    session_dir=$(get_session_directory "$session_id" "$base_dir")
    
    if [[ ! -d "$session_dir" ]]; then
        session_error "Session directory does not exist: $session_dir"
        return 1
    fi
    
    local state_file="$session_dir/$STATE_FILE_NAME"
    
    if [[ -f "$state_file" ]]; then
        session_warn "State file already exists: $state_file"
        echo "$state_file"
        return 0
    fi
    
    # Create initial state with metadata
    python3 << EOF
import json
from datetime import datetime

state = json.loads('''$(get_default_session_state)''')
state['timestamp'] = datetime.now().isoformat()
state['sessionId'] = '$session_id'

with open('$state_file', 'w') as f:
    json.dump(state, f, indent=2)
EOF
    
    if [[ $? -eq 0 ]]; then
        session_log "Initialized state file: $state_file"
        echo "$state_file"
    else
        session_error "Failed to initialize state file: $state_file"
        return 1
    fi
}

# Save session state with versioning and backup
save_session_state() {
    local session_id="$1"
    local state_data="$2"
    local base_dir="${3:-.flowloom}"
    
    session_debug "Saving state for session: $session_id"
    
    if ! validate_session_id "$session_id"; then
        session_error "Invalid session ID: $session_id"
        return 1
    fi
    
    local session_dir
    session_dir=$(get_session_directory "$session_id" "$base_dir")
    
    local state_file="$session_dir/$STATE_FILE_NAME"
    
    # Create backup if state file exists
    if [[ -f "$state_file" ]]; then
        create_state_backup "$state_file" "$session_id" "$base_dir"
    fi
    
    # Prepare state with metadata
    python3 << EOF
import json
import sys
from datetime import datetime

try:
    # Parse the provided state data
    if '$state_data'.startswith('{'):
        # JSON string provided
        new_data = json.loads('''$state_data''')
    else:
        # File path provided
        with open('$state_data', 'r') as f:
            new_data = json.load(f)
    
    # Create state with metadata
    state_with_metadata = {
        "version": $STATE_VERSION,
        "timestamp": datetime.now().isoformat(),
        "sessionId": "$session_id",
        "data": new_data
    }
    
    # Validate state structure
    required_fields = ["currentPlan", "activeCommands", "recentFiles", "gitState", "modeHistory", "contextHistory"]
    for field in required_fields:
        if field not in new_data:
            print(f"Warning: Missing required field: {field}", file=sys.stderr)
            if field == "gitState":
                new_data[field] = {"branch": "main", "uncommittedFiles": [], "lastCommit": None}
            else:
                new_data[field] = [] if field.endswith("s") or field.endswith("y") else None
    
    # Atomic write using temporary file
    temp_file = '$state_file.tmp'
    with open(temp_file, 'w') as f:
        json.dump(state_with_metadata, f, indent=2)
    
    # Move temp file to final location
    import os
    os.rename(temp_file, '$state_file')
    
    print("success")
    
except Exception as e:
    print(f"Error saving state: {e}", file=sys.stderr)
    sys.exit(1)
EOF
    
    local result=$?
    if [[ $result -eq 0 ]]; then
        session_log "Saved state for session: $session_id"
        echo "$state_file"
    else
        session_error "Failed to save state for session: $session_id"
        return 1
    fi
}

# Load session state
load_session_state() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    local output_format="${3:-json}"  # json, data, or file
    
    session_debug "Loading state for session: $session_id"
    
    if ! validate_session_id "$session_id"; then
        session_error "Invalid session ID: $session_id"
        return 1
    fi
    
    local session_dir
    session_dir=$(get_session_directory "$session_id" "$base_dir")
    
    local state_file="$session_dir/$STATE_FILE_NAME"
    
    if [[ ! -f "$state_file" ]]; then
        session_warn "State file not found, initializing: $state_file"
        initialize_session_state "$session_id" "$base_dir" >/dev/null
    fi
    
    case "$output_format" in
        "file")
            echo "$state_file"
            ;;
        "data")
            python3 << EOF
import json
import sys

try:
    with open('$state_file', 'r') as f:
        state = json.load(f)
    
    # Validate state format
    if 'data' not in state:
        print("Error: Invalid state format - missing 'data' field", file=sys.stderr)
        sys.exit(1)
    
    print(json.dumps(state['data'], indent=2))
    
except Exception as e:
    print(f"Error loading state: {e}", file=sys.stderr)
    sys.exit(1)
EOF
            ;;
        "json"|*)
            cat "$state_file"
            ;;
    esac
}

# Update specific field in session state
update_session_state_field() {
    local session_id="$1"
    local field_path="$2"
    local new_value="$3"
    local base_dir="${4:-.flowloom}"
    
    session_debug "Updating state field '$field_path' for session: $session_id"
    
    local session_dir
    session_dir=$(get_session_directory "$session_id" "$base_dir")
    
    local state_file="$session_dir/$STATE_FILE_NAME"
    
    if [[ ! -f "$state_file" ]]; then
        session_error "State file not found: $state_file"
        return 1
    fi
    
    # Create backup before updating
    create_state_backup "$state_file" "$session_id" "$base_dir"
    
    python3 << EOF
import json
import sys
from datetime import datetime

try:
    with open('$state_file', 'r') as f:
        state = json.load(f)
    
    # Navigate to the field using dot notation
    field_parts = '$field_path'.split('.')
    current = state['data']
    
    # Navigate to parent of target field
    for part in field_parts[:-1]:
        if part not in current:
            current[part] = {}
        current = current[part]
    
    # Set the final field
    field_name = field_parts[-1]
    
    # Parse value based on type
    if '$new_value' == 'null':
        current[field_name] = None
    elif '$new_value' == 'true':
        current[field_name] = True
    elif '$new_value' == 'false':
        current[field_name] = False
    elif '$new_value'.startswith('[') or '$new_value'.startswith('{'):
        current[field_name] = json.loads('''$new_value''')
    elif '$new_value'.isdigit():
        current[field_name] = int('$new_value')
    else:
        current[field_name] = '$new_value'
    
    # Update metadata
    state['timestamp'] = datetime.now().isoformat()
    
    # Atomic write
    temp_file = '$state_file.tmp'
    with open(temp_file, 'w') as f:
        json.dump(state, f, indent=2)
    
    import os
    os.rename(temp_file, '$state_file')
    
    print("success")
    
except Exception as e:
    print(f"Error updating state field: {e}", file=sys.stderr)
    sys.exit(1)
EOF
    
    local result=$?
    if [[ $result -eq 0 ]]; then
        session_log "Updated state field '$field_path' for session: $session_id"
    else
        session_error "Failed to update state field '$field_path' for session: $session_id"
        return 1
    fi
}

# Get specific field from session state
get_session_state_field() {
    local session_id="$1"
    local field_path="$2"
    local default_value="${3:-null}"
    local base_dir="${4:-.flowloom}"
    
    local session_dir
    session_dir=$(get_session_directory "$session_id" "$base_dir")
    
    local state_file="$session_dir/$STATE_FILE_NAME"
    
    if [[ ! -f "$state_file" ]]; then
        echo "$default_value"
        return 0
    fi
    
    python3 << EOF
import json
import sys

try:
    with open('$state_file', 'r') as f:
        state = json.load(f)
    
    # Navigate to the field using dot notation
    field_parts = '$field_path'.split('.')
    current = state['data']
    
    for part in field_parts:
        if part not in current:
            print('$default_value')
            sys.exit(0)
        current = current[part]
    
    if isinstance(current, (dict, list)):
        print(json.dumps(current))
    else:
        print(current)
    
except Exception as e:
    print('$default_value')
    sys.exit(0)
EOF
}

# Create state backup
create_state_backup() {
    local state_file="$1"
    local session_id="$2"
    local base_dir="${3:-.flowloom}"
    
    if [[ ! -f "$state_file" ]]; then
        return 0
    fi
    
    local session_dir
    session_dir=$(get_session_directory "$session_id" "$base_dir")
    
    local backup_dir="$session_dir/backups"
    mkdir -p "$backup_dir"
    
    local timestamp
    timestamp=$(date +%Y%m%d_%H%M%S)
    local backup_file="$backup_dir/state_${timestamp}.json"
    
    cp "$state_file" "$backup_file"
    
    # Cleanup old backups (keep only MAX_STATE_BACKUPS)
    local backup_count
    backup_count=$(find "$backup_dir" -name "state_*.json" | wc -l)
    
    if [[ $backup_count -gt $MAX_STATE_BACKUPS ]]; then
        find "$backup_dir" -name "state_*.json" -type f -printf '%T+ %p\n' | sort | head -n $((backup_count - MAX_STATE_BACKUPS)) | cut -d' ' -f2- | xargs rm -f
    fi
    
    session_debug "Created state backup: $backup_file"
    echo "$backup_file"
}

# Recover state from backup
recover_session_state() {
    local session_id="$1"
    local backup_timestamp="${2:-latest}"
    local base_dir="${3:-.flowloom}"
    
    session_debug "Recovering state for session: $session_id (backup: $backup_timestamp)"
    
    local session_dir
    session_dir=$(get_session_directory "$session_id" "$base_dir")
    
    local backup_dir="$session_dir/backups"
    local state_file="$session_dir/$STATE_FILE_NAME"
    
    if [[ ! -d "$backup_dir" ]]; then
        session_error "No backup directory found: $backup_dir"
        return 1
    fi
    
    local backup_file
    if [[ "$backup_timestamp" == "latest" ]]; then
        backup_file=$(find "$backup_dir" -name "state_*.json" -type f -printf '%T+ %p\n' | sort -r | head -n1 | cut -d' ' -f2-)
    else
        backup_file="$backup_dir/state_${backup_timestamp}.json"
    fi
    
    if [[ ! -f "$backup_file" ]]; then
        session_error "Backup file not found: $backup_file"
        return 1
    fi
    
    # Validate backup before restoring
    if validate_json_file "$backup_file"; then
        cp "$backup_file" "$state_file"
        session_log "Recovered state from backup: $backup_file"
        echo "$state_file"
    else
        session_error "Backup file is corrupted: $backup_file"
        return 1
    fi
}

# List available state backups
list_state_backups() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    
    local session_dir
    session_dir=$(get_session_directory "$session_id" "$base_dir")
    
    local backup_dir="$session_dir/backups"
    
    if [[ ! -d "$backup_dir" ]]; then
        return 0
    fi
    
    find "$backup_dir" -name "state_*.json" -type f -printf '%T+ %p\n' | sort -r | while read -r timestamp file; do
        local backup_name
        backup_name=$(basename "$file" .json | sed 's/state_//')
        local size
        size=$(stat -c%s "$file")
        echo "$backup_name $size $timestamp"
    done
}

# Validate state file structure
validate_session_state() {
    local state_file="$1"
    
    if [[ ! -f "$state_file" ]]; then
        session_error "State file not found: $state_file"
        return 1
    fi
    
    python3 << EOF
import json
import sys

try:
    with open('$state_file', 'r') as f:
        state = json.load(f)
    
    # Check required top-level fields
    required_top_fields = ['version', 'timestamp', 'sessionId', 'data']
    for field in required_top_fields:
        if field not in state:
            print(f"Missing required field: {field}", file=sys.stderr)
            sys.exit(1)
    
    # Check required data fields
    data = state['data']
    required_data_fields = ['currentPlan', 'activeCommands', 'recentFiles', 'gitState', 'modeHistory', 'contextHistory']
    for field in required_data_fields:
        if field not in data:
            print(f"Missing required data field: {field}", file=sys.stderr)
            sys.exit(1)
    
    # Validate gitState structure
    git_state = data['gitState']
    if not isinstance(git_state, dict):
        print("gitState must be an object", file=sys.stderr)
        sys.exit(1)
    
    required_git_fields = ['branch', 'uncommittedFiles', 'lastCommit']
    for field in required_git_fields:
        if field not in git_state:
            print(f"Missing gitState field: {field}", file=sys.stderr)
            sys.exit(1)
    
    print("valid")
    
except json.JSONDecodeError as e:
    print(f"Invalid JSON: {e}", file=sys.stderr)
    sys.exit(1)
except Exception as e:
    print(f"Validation error: {e}", file=sys.stderr)
    sys.exit(1)
EOF
}

# Print help
print_state_help() {
    cat << EOF
FlowLoom Session State Management

Usage: session_state.sh <command> [options]

Commands:
  init <session_id> [base_dir]                 Initialize state for session
  save <session_id> <state_data> [base_dir]   Save session state (JSON string or file)
  load <session_id> [base_dir] [format]       Load session state (json|data|file)
  update <session_id> <field> <value> [base_dir]  Update specific state field
  get <session_id> <field> [default] [base_dir]   Get specific state field
  backup <session_id> [base_dir]              Create state backup
  recover <session_id> [timestamp] [base_dir] Recover from backup
  list-backups <session_id> [base_dir]        List available backups
  validate <state_file>                       Validate state file structure

Examples:
  session_state.sh init session_123_456_abc123
  session_state.sh update session_123_456_abc123 currentPlan "plan_5120"
  session_state.sh get session_123_456_abc123 gitState.branch
  session_state.sh backup session_123_456_abc123
  session_state.sh recover session_123_456_abc123 latest

Notes:
  - Field paths use dot notation (e.g., gitState.branch)
  - JSON values are automatically parsed by type
  - Atomic operations with automatic backup
  - Respects Claude Code security boundaries
EOF
}

# Main command dispatcher
main() {
    if [[ $# -eq 0 ]]; then
        print_state_help
        exit 1
    fi
    
    local command="$1"
    shift
    
    case "$command" in
        "init")
            if [[ $# -lt 1 ]]; then
                echo "Error: init requires session_id" >&2
                exit 1
            fi
            initialize_session_state "$@"
            ;;
        "save")
            if [[ $# -lt 2 ]]; then
                echo "Error: save requires session_id and state_data" >&2
                exit 1
            fi
            save_session_state "$@"
            ;;
        "load")
            if [[ $# -lt 1 ]]; then
                echo "Error: load requires session_id" >&2
                exit 1
            fi
            load_session_state "$@"
            ;;
        "update")
            if [[ $# -lt 3 ]]; then
                echo "Error: update requires session_id, field, and value" >&2
                exit 1
            fi
            update_session_state_field "$@"
            ;;
        "get")
            if [[ $# -lt 2 ]]; then
                echo "Error: get requires session_id and field" >&2
                exit 1
            fi
            get_session_state_field "$@"
            ;;
        "backup")
            if [[ $# -lt 1 ]]; then
                echo "Error: backup requires session_id" >&2
                exit 1
            fi
            create_state_backup "$(get_session_directory "$1" "${2:-.flowloom}")/$STATE_FILE_NAME" "$@"
            ;;
        "recover")
            if [[ $# -lt 1 ]]; then
                echo "Error: recover requires session_id" >&2
                exit 1
            fi
            recover_session_state "$@"
            ;;
        "list-backups")
            if [[ $# -lt 1 ]]; then
                echo "Error: list-backups requires session_id" >&2
                exit 1
            fi
            list_state_backups "$@"
            ;;
        "validate")
            if [[ $# -lt 1 ]]; then
                echo "Error: validate requires state_file" >&2
                exit 1
            fi
            validate_session_state "$@"
            ;;
        "help"|"--help"|"-h")
            print_state_help
            ;;
        *)
            echo "Error: Unknown command: $command" >&2
            echo "Use 'session_state.sh help' for usage information" >&2
            exit 1
            ;;
    esac
}

# Run main function if script is executed directly
if [[ "${BASH_SOURCE[0]:-$0}" == "${0}" ]]; then
    main "$@"
fi