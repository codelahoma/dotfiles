#!/bin/bash
# bin/session_identifier.sh - Session identification utilities
# Part of FlowLoom Session Management System
# Based on Plan 3120 - Session Tracking Detailed Design
#
# SECURITY & ETHICS NOTICE:
# This script respects Claude Code's security boundaries and should never be used
# to evade or circumvent security measures. PPID access is used solely for legitimate
# session tracking within approved workflows. Users should maintain the same ethical
# standards and respect all security boundaries in their implementations.
#
# PPID Usage: Only for session identification within the same process tree,
# never for unauthorized process inspection or security bypass attempts.

set -euo pipefail

# Source session utilities for error handling
if [[ -z "${SCRIPT_DIR:-}" ]]; then
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
fi
# Only source session_utils if not already loaded to prevent circular dependency
if [[ -z "${SESSION_UTILS_LOADED:-}" ]]; then
    source "${SCRIPT_DIR}/session_utils.sh" 2>/dev/null || true
fi

# Create unique session ID with purpose
create_session_id() {
    local purpose="${1:-development}"
    local ppid="${2:-${PPID:-$$}}"
    
    # Sanitize purpose: lowercase, replace spaces/special chars with hyphens, limit length
    local clean_purpose
    clean_purpose=$(echo "$purpose" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9-]/-/g' | sed 's/--\+/-/g' | sed 's/^-\|-$//g' | cut -c1-20)
    
    echo "${ppid}-${clean_purpose}"
}

# Create legacy random session ID (for backwards compatibility)
create_legacy_session_id() {
    local ppid="${PPID:-$$}"
    local timestamp=$(date +%s)
    local random_suffix=$(LC_ALL=C head -c 1000 /dev/urandom | LC_ALL=C tr -dc 'a-z0-9' | head -c6)
    echo "${ppid}-${random_suffix}"
}

# Extract PPID from session ID
extract_ppid_from_session_id() {
    local session_id="$1"
    echo "${session_id%-*}"
}

# Extract purpose from session ID (new format only)
extract_purpose_from_session_id() {
    local session_id="$1"
    local suffix="${session_id#*-}"
    
    # Check if it's legacy format (6 chars, all lowercase alphanumeric)
    if [[ "$suffix" =~ ^[a-z0-9]{6}$ ]]; then
        echo "legacy"
    else
        echo "$suffix"
    fi
}

# Check if session ID is legacy format
is_legacy_session_id() {
    local session_id="$1"
    local suffix="${session_id#*-}"
    [[ "$suffix" =~ ^[a-z0-9]{6}$ ]]
}

# Create session branch name from ID (deprecated - main-only workflow)
create_session_branch_name() {
    local session_id="$1"
    echo "main"  # All sessions now use main branch
}

# Create session worktree name from ID (deprecated - main-only workflow)  
create_session_worktree_name() {
    local session_id="$1"
    echo "session-${session_id}-deprecated"  # Worktrees no longer used
}

# Validate session ID format (PPID-purpose or PPID-suffix)
validate_session_id() {
    local session_id="$1"
    # New format: PPID-purpose (e.g., 58883-session-management)
    if [[ "$session_id" =~ ^[0-9]+-[a-z0-9]([a-z0-9-]*[a-z0-9])?$ ]]; then
        return 0
    # Legacy format: PPID-6char (e.g., 58883-abc123)
    elif [[ "$session_id" =~ ^[0-9]+-[a-z0-9][a-z0-9][a-z0-9][a-z0-9][a-z0-9][a-z0-9]$ ]]; then
        return 0
    else
        return 1
    fi
}

# Get current PPID
# SECURITY NOTE: This function accesses PPID for legitimate session tracking only.
# It respects Claude Code security boundaries and should never be used for unauthorized
# process inspection or to circumvent security measures.
get_current_ppid() {
    # Ethical use: PPID access for session management within approved workflows
    echo "${PPID:-$}"
}

# Extract timestamp from session ID (deprecated - legacy format only)
extract_timestamp_from_session_id() {
    local session_id="$1"
    # Only works with very old session format - return empty for new format
    echo "$session_id" | sed -n 's/^session_[0-9]\+_\([0-9]\+\)_.*/\1/p'
}

# Extract random suffix from session ID (deprecated - legacy format only)
extract_suffix_from_session_id() {
    local session_id="$1"
    # Only works with very old session format - return empty for new format
    echo "$session_id" | sed -n 's/^session_[0-9]\+_[0-9]\+_\([a-z0-9]\{6\}\)$/\1/p'
}

# Generate session ID for specific PPID (for testing) - updated for new format
create_session_id_for_ppid() {
    local ppid="$1"
    local purpose="${2:-testing}"
    create_session_id "$purpose" "$ppid"
}

# Check if session ID belongs to current process tree
is_session_id_current() {
    local session_id="$1"
    local current_ppid="${PPID:-$$}"
    local session_ppid
    
    session_ppid=$(extract_ppid_from_session_id "$session_id")
    [[ "$session_ppid" == "$current_ppid" ]]
}

# Get session age in seconds (deprecated - requires metadata file for new format)
get_session_age() {
    local session_id="$1"
    local session_timestamp
    
    # Try legacy format first
    session_timestamp=$(extract_timestamp_from_session_id "$session_id")
    
    if [[ -n "$session_timestamp" ]]; then
        local current_timestamp=$(date +%s)
        echo $((current_timestamp - session_timestamp))
    else
        # New format doesn't embed timestamp - return 0 or get from metadata
        echo "0"
    fi
}

# Format session age for human reading (deprecated - requires metadata file for new format)
format_session_age() {
    local session_id="$1"
    local age_seconds
    
    age_seconds=$(get_session_age "$session_id")
    
    if [[ "$age_seconds" == "0" ]]; then
        echo "unknown (use session metadata for new format)"
    elif [[ $age_seconds -lt 60 ]]; then
        echo "${age_seconds} seconds"
    elif [[ $age_seconds -lt 3600 ]]; then
        echo "$((age_seconds / 60)) minutes"
    elif [[ $age_seconds -lt 86400 ]]; then
        echo "$((age_seconds / 3600)) hours"
    else
        echo "$((age_seconds / 86400)) days"
    fi
}

# Main execution for testing
if [[ "${BASH_SOURCE[0]:-$0}" == "${0}" ]]; then
    case "${1:-}" in
        "create")
            create_session_id "${2:-development}" "${3:-}"
            ;;
        "validate")
            if [[ -z "${2:-}" ]]; then
                echo "Usage: $0 validate <session_id>" >&2
                exit 1
            fi
            if validate_session_id "$2"; then
                echo "Valid session ID: $2"
                exit 0
            else
                echo "Invalid session ID: $2" >&2
                exit 1
            fi
            ;;
        "extract-ppid")
            if [[ -z "${2:-}" ]]; then
                echo "Usage: $0 extract-ppid <session_id>" >&2
                exit 1
            fi
            extract_ppid_from_session_id "$2"
            ;;
        "current-ppid")
            get_current_ppid
            ;;
        "age")
            if [[ -z "${2:-}" ]]; then
                echo "Usage: $0 age <session_id>" >&2
                exit 1
            fi
            format_session_age "$2"
            ;;
        "test")
            echo "Testing session identifier functions..."
            session_id=$(create_session_id)
            echo "Created: $session_id"
            echo "Valid: $(validate_session_id "$session_id" && echo "yes" || echo "no")"
            echo "PPID: $(extract_ppid_from_session_id "$session_id")"
            echo "Timestamp: $(extract_timestamp_from_session_id "$session_id")"
            echo "Suffix: $(extract_suffix_from_session_id "$session_id")"
            echo "Age: $(format_session_age "$session_id")"
            ;;
        *)
            echo "FlowLoom Session Identifier Utilities"
            echo "Usage: $0 {create|validate|extract-ppid|current-ppid|age|test} [args]"
            echo ""
            echo "Commands:"
            echo "  create              - Create new session ID"
            echo "  validate <id>       - Validate session ID format"
            echo "  extract-ppid <id>   - Extract PPID from session ID"
            echo "  current-ppid        - Get current process PPID"
            echo "  age <id>            - Get human-readable session age"
            echo "  test                - Run basic functionality test"
            exit 1
            ;;
    esac
fi