#!/bin/bash
# bin/session_filesystem.sh - Session file system utilities
# Part of FlowLoom Session Management System
# Based on Plan 3120 - Session Tracking Detailed Design
#
# SECURITY & ETHICS NOTICE:
# This script respects Claude Code's security boundaries and operates only within
# approved directory structures. File operations are limited to session management
# within the project directory and should never be used to access unauthorized
# locations or circumvent security measures.

set -euo pipefail

# Source session utilities for error handling
if [[ -z "${SCRIPT_DIR:-}" ]]; then
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
fi
# Only source session_utils if not already loaded to prevent circular dependency
if [[ -z "${SESSION_UTILS_LOADED:-}" ]]; then
    source "${SCRIPT_DIR}/session_utils.sh" 2>/dev/null || true
fi

# Initialize session directory structure
initialize_session_structure() {
    local base_dir="${1:-.flowloom}"
    
    # Ensure we're operating within safe boundaries
    if [[ "$base_dir" =~ ^/ ]] && [[ ! "$base_dir" =~ ^"$(pwd)" ]]; then
        session_error "Security: Will not create session structure outside project directory"
        return 1
    fi
    
    # Create directory structure
    mkdir -p "$base_dir/sessions"
    mkdir -p "$base_dir/config"
    
    # Initialize index file if it doesn't exist
    local index_file="$base_dir/sessions/index.json"
    if [[ ! -f "$index_file" ]]; then
        cat > "$index_file" << EOF
{
  "sessions": {},
  "lastUpdated": "$(date -Iseconds)",
  "version": "1.0",
  "securityNote": "FlowLoom session index - respects Claude Code security boundaries"
}
EOF
        session_log "Created session index: $index_file"
    fi
    
    echo "$base_dir"
}

# Create session directory
create_session_directory() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    
    # Validate session ID format for security
    if ! validate_session_id "$session_id" 2>/dev/null; then
        session_error "Invalid session ID format: $session_id"
        return 1
    fi
    
    local session_dir="$base_dir/sessions/$session_id"
    local artifacts_dir="$session_dir/artifacts"
    
    # Create directories
    mkdir -p "$session_dir"
    mkdir -p "$artifacts_dir"
    
    # Create basic session structure
    touch "$session_dir/metadata.json"
    touch "$session_dir/state.json"
    
    session_log "Created session directory: $session_dir"
    echo "$session_dir"
}

# Check if session exists
session_exists() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    
    [[ -d "$base_dir/sessions/$session_id" ]]
}

# Get session directory path
get_session_directory() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    
    echo "$base_dir/sessions/$session_id"
}

# Get session metadata file path
get_session_metadata_file() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    
    echo "$base_dir/sessions/$session_id/metadata.json"
}

# Get session state file path
get_session_state_file() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    
    echo "$base_dir/sessions/$session_id/state.json"
}

# List all session directories
list_session_directories() {
    local base_dir="${1:-.flowloom}"
    
    if [[ ! -d "$base_dir/sessions" ]]; then
        return 0
    fi
    
    find "$base_dir/sessions" -maxdepth 1 -type d -name "session_*" | sort
}

# Get session directory size
get_session_directory_size() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    local session_dir="$base_dir/sessions/$session_id"
    
    if [[ ! -d "$session_dir" ]]; then
        echo "0"
        return 1
    fi
    
    du -sh "$session_dir" | cut -f1
}

# Clean up old session directories (older than specified days)
cleanup_old_sessions() {
    local base_dir="${1:-.flowloom}"
    local days_old="${2:-30}"
    local dry_run="${3:-false}"
    
    session_log "Cleaning up sessions older than $days_old days (dry_run: $dry_run)"
    
    if [[ ! -d "$base_dir/sessions" ]]; then
        return 0
    fi
    
    local cleaned_count=0
    
    while IFS= read -r -d '' session_dir; do
        local session_id
        session_id=$(basename "$session_dir")
        
        # Extract timestamp from session ID for age check
        local timestamp
        timestamp=$(echo "$session_id" | sed -n 's/^session_[0-9]\+_\([0-9]\+\)_.*/\1/p')
        
        if [[ -n "$timestamp" ]]; then
            local current_time
            current_time=$(date +%s)
            local age_seconds=$((current_time - timestamp))
            local age_days=$((age_seconds / 86400))
            
            if [[ $age_days -gt $days_old ]]; then
                if [[ "$dry_run" == "true" ]]; then
                    echo "Would clean: $session_dir (${age_days} days old)"
                else
                    session_log "Cleaning session: $session_id (${age_days} days old)"
                    rm -rf "$session_dir"
                fi
                cleaned_count=$((cleaned_count + 1))
            fi
        fi
    done < <(find "$base_dir/sessions" -maxdepth 1 -type d -name "session_*" -print0)
    
    session_log "Cleaned $cleaned_count old sessions"
    echo "$cleaned_count"
}

# Create session backup
backup_session() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    local backup_dir="${3:-$base_dir/backups}"
    
    local session_dir="$base_dir/sessions/$session_id"
    local backup_file="$backup_dir/session_${session_id}_$(date +%Y%m%d_%H%M%S).tar.gz"
    
    if [[ ! -d "$session_dir" ]]; then
        session_error "Session directory not found: $session_dir"
        return 1
    fi
    
    mkdir -p "$backup_dir"
    
    # Create compressed backup
    tar -czf "$backup_file" -C "$base_dir/sessions" "$session_id"
    
    session_log "Created session backup: $backup_file"
    echo "$backup_file"
}

# Restore session from backup
restore_session_from_backup() {
    local backup_file="$1"
    local base_dir="${2:-.flowloom}"
    
    if [[ ! -f "$backup_file" ]]; then
        session_error "Backup file not found: $backup_file"
        return 1
    fi
    
    # Extract to sessions directory
    tar -xzf "$backup_file" -C "$base_dir/sessions"
    
    session_log "Restored session from backup: $backup_file"
}

# Validate session directory structure
validate_session_structure() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    local session_dir="$base_dir/sessions/$session_id"
    
    local validation_errors=()
    
    # Check if session directory exists
    if [[ ! -d "$session_dir" ]]; then
        validation_errors+=("Session directory missing: $session_dir")
    fi
    
    # Check for required files
    if [[ ! -f "$session_dir/metadata.json" ]]; then
        validation_errors+=("Missing metadata.json")
    fi
    
    if [[ ! -f "$session_dir/state.json" ]]; then
        validation_errors+=("Missing state.json")
    fi
    
    if [[ ! -d "$session_dir/artifacts" ]]; then
        validation_errors+=("Missing artifacts directory")
    fi
    
    if [[ ${#validation_errors[@]} -eq 0 ]]; then
        return 0
    else
        printf '%s\n' "${validation_errors[@]}" >&2
        return 1
    fi
}

# Main execution for testing
if [[ "${BASH_SOURCE[0]:-$0}" == "${0}" ]]; then
    case "${1:-}" in
        "init")
            base_dir="${2:-.flowloom}"
            initialize_session_structure "$base_dir"
            ;;
        "create")
            if [[ -z "${2:-}" ]]; then
                echo "Usage: $0 create <session_id> [base_dir]" >&2
                exit 1
            fi
            create_session_directory "$2" "${3:-.flowloom}"
            ;;
        "exists")
            if [[ -z "${2:-}" ]]; then
                echo "Usage: $0 exists <session_id> [base_dir]" >&2
                exit 1
            fi
            if session_exists "$2" "${3:-.flowloom}"; then
                echo "Session exists: $2"
                exit 0
            else
                echo "Session does not exist: $2" >&2
                exit 1
            fi
            ;;
        "list")
            list_session_directories "${2:-.flowloom}"
            ;;
        "size")
            if [[ -z "${2:-}" ]]; then
                echo "Usage: $0 size <session_id> [base_dir]" >&2
                exit 1
            fi
            get_session_directory_size "$2" "${3:-.flowloom}"
            ;;
        "cleanup")
            cleanup_old_sessions "${2:-.flowloom}" "${3:-30}" "${4:-false}"
            ;;
        "backup")
            if [[ -z "${2:-}" ]]; then
                echo "Usage: $0 backup <session_id> [base_dir] [backup_dir]" >&2
                exit 1
            fi
            backup_session "$2" "${3:-.flowloom}" "${4:-${3:-.flowloom}/backups}"
            ;;
        "validate")
            if [[ -z "${2:-}" ]]; then
                echo "Usage: $0 validate <session_id> [base_dir]" >&2
                exit 1
            fi
            if validate_session_structure "$2" "${3:-.flowloom}"; then
                echo "Session structure valid: $2"
                exit 0
            else
                echo "Session structure invalid: $2" >&2
                exit 1
            fi
            ;;
        "test")
            echo "Testing session filesystem functions..."
            test_dir="/tmp/test_flowloom_session_$$"
            echo "Using test directory: $test_dir"
            
            # Test initialization
            echo "Initializing session structure..."
            initialize_session_structure "$test_dir"
            
            # Test session creation
            echo "Creating test session..."
            session_id="session_12345_$(date +%s)_test01"
            create_session_directory "$session_id" "$test_dir"
            
            # Test validation
            echo "Validating session structure..."
            validate_session_structure "$session_id" "$test_dir"
            
            # Test listing
            echo "Listing sessions:"
            list_session_directories "$test_dir"
            
            # Cleanup
            rm -rf "$test_dir"
            echo "Test completed successfully"
            ;;
        *)
            echo "FlowLoom Session Filesystem Utilities"
            echo "Usage: $0 {init|create|exists|list|size|cleanup|backup|validate|test} [args]"
            echo ""
            echo "Commands:"
            echo "  init [base_dir]                    - Initialize session structure"
            echo "  create <session_id> [base_dir]     - Create session directory"
            echo "  exists <session_id> [base_dir]     - Check if session exists"
            echo "  list [base_dir]                    - List all session directories"
            echo "  size <session_id> [base_dir]       - Get session directory size"
            echo "  cleanup [base_dir] [days] [dry]    - Clean up old sessions"
            echo "  backup <session_id> [base] [backup] - Backup session"
            echo "  validate <session_id> [base_dir]   - Validate session structure"
            echo "  test                               - Run basic functionality test"
            exit 1
            ;;
    esac
fi