#!/bin/bash
# bin/session_index.sh - Session index management with JSON
# Part of FlowLoom Session Management System
# Based on Plan 3120 - Session Tracking Detailed Design
#
# SECURITY & ETHICS NOTICE:
# This script manages session indexing while respecting Claude Code's security
# boundaries. All session data access is limited to approved workflows and
# legitimate session management purposes. Never use for unauthorized data access.

set -euo pipefail

# Source session utilities
if [[ -z "${SCRIPT_DIR:-}" ]]; then
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
fi
source "${SCRIPT_DIR}/session_utils.sh"
source "${SCRIPT_DIR}/session_identifier.sh"

# Get session index file path
get_session_index_file() {
    local base_dir="${1:-.flowloom}"
    echo "$base_dir/sessions/index.json"
}

# Initialize session index
initialize_session_index() {
    local base_dir="${1:-.flowloom}"
    local index_file
    index_file=$(get_session_index_file "$base_dir")
    
    # Create directory if it doesn't exist
    mkdir -p "$(dirname "$index_file")"
    
    # Create index if it doesn't exist
    if [[ ! -f "$index_file" ]]; then
        cat > "$index_file" << EOF
{
  "sessions": {},
  "lastUpdated": "$(date -Iseconds)",
  "version": "1.0",
  "securityNote": "FlowLoom session index - respects Claude Code security boundaries",
  "totalSessions": 0,
  "activeSessions": 0
}
EOF
        session_log "Initialized session index: $index_file"
    fi
    
    echo "$index_file"
}

# Add session to index
add_session_to_index() {
    local session_id="$1"
    local metadata_file="$2"
    local base_dir="${3:-.flowloom}"
    
    # Validate inputs
    if ! validate_session_id "$session_id"; then
        return 1
    fi
    
    if [[ ! -f "$metadata_file" ]]; then
        session_error "Metadata file not found: $metadata_file"
        return 1
    fi
    
    local index_file
    index_file=$(get_session_index_file "$base_dir")
    
    # Initialize index if needed
    if [[ ! -f "$index_file" ]]; then
        initialize_session_index "$base_dir" >/dev/null
    fi
    
    # Extract key metadata fields for index
    local ppid start_time status working_dir git_branch last_activity
    
    ppid=$(get_json_field "$metadata_file" "ppid" "0")
    start_time=$(get_json_field "$metadata_file" "startTime" "")
    status=$(get_json_field "$metadata_file" "status" "unknown")
    working_dir=$(get_json_field "$metadata_file" "workingDirectory" "")
    git_branch=$(get_json_field "$metadata_file" "gitBranch" "")
    last_activity=$(get_json_field "$metadata_file" "lastActivity" "")
    
    # Create index entry using Python for robust JSON manipulation
    python3 << EOF
import json
import sys
from datetime import datetime

try:
    # Read current index
    with open('$index_file', 'r') as f:
        index = json.load(f)
    
    # Create session entry
    session_entry = {
        "sessionId": "$session_id",
        "ppid": int($ppid) if "$ppid".isdigit() else 0,
        "status": "$status",
        "startTime": "$start_time",
        "lastActivity": "$last_activity",
        "workingDirectory": "$working_dir",
        "gitBranch": "$git_branch",
        "addedToIndex": datetime.now().isoformat()
    }
    
    # Add to sessions
    index["sessions"]["$session_id"] = session_entry
    
    # Update counters
    index["totalSessions"] = len(index["sessions"])
    index["activeSessions"] = sum(1 for s in index["sessions"].values() if s.get("status") == "active")
    index["lastUpdated"] = datetime.now().isoformat()
    
    # Write back
    with open('$index_file', 'w') as f:
        json.dump(index, f, indent=2)
    
    print("Session added to index successfully")
    
except Exception as e:
    print(f"Error adding session to index: {e}", file=sys.stderr)
    sys.exit(1)
EOF
    
    session_log "Added session to index: $session_id"
}

# Remove session from index
remove_session_from_index() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    
    local index_file
    index_file=$(get_session_index_file "$base_dir")
    
    if [[ ! -f "$index_file" ]]; then
        session_warn "Session index not found: $index_file"
        return 1
    fi
    
    python3 << EOF
import json
import sys
from datetime import datetime

try:
    # Read current index
    with open('$index_file', 'r') as f:
        index = json.load(f)
    
    # Remove session if it exists
    if "$session_id" in index["sessions"]:
        del index["sessions"]["$session_id"]
        
        # Update counters
        index["totalSessions"] = len(index["sessions"])
        index["activeSessions"] = sum(1 for s in index["sessions"].values() if s.get("status") == "active")
        index["lastUpdated"] = datetime.now().isoformat()
        
        # Write back
        with open('$index_file', 'w') as f:
            json.dump(index, f, indent=2)
        
        print("Session removed from index successfully")
    else:
        print("Session not found in index: $session_id", file=sys.stderr)
        sys.exit(1)
    
except Exception as e:
    print(f"Error removing session from index: {e}", file=sys.stderr)
    sys.exit(1)
EOF
    
    session_log "Removed session from index: $session_id"
}

# Update session status in index
update_session_status() {
    local session_id="$1"
    local new_status="$2"
    local base_dir="${3:-.flowloom}"
    
    local index_file
    index_file=$(get_session_index_file "$base_dir")
    
    if [[ ! -f "$index_file" ]]; then
        session_error "Session index not found: $index_file"
        return 1
    fi
    
    python3 << EOF
import json
import sys
from datetime import datetime

try:
    # Read current index
    with open('$index_file', 'r') as f:
        index = json.load(f)
    
    # Update session status if it exists
    if "$session_id" in index["sessions"]:
        index["sessions"]["$session_id"]["status"] = "$new_status"
        index["sessions"]["$session_id"]["lastActivity"] = datetime.now().isoformat()
        
        # Update counters
        index["activeSessions"] = sum(1 for s in index["sessions"].values() if s.get("status") == "active")
        index["lastUpdated"] = datetime.now().isoformat()
        
        # Write back
        with open('$index_file', 'w') as f:
            json.dump(index, f, indent=2)
        
        print("Session status updated successfully")
    else:
        print("Session not found in index: $session_id", file=sys.stderr)
        sys.exit(1)
    
except Exception as e:
    print(f"Error updating session status: {e}", file=sys.stderr)
    sys.exit(1)
EOF
    
    session_log "Updated session status: $session_id -> $new_status"
}

# Find sessions by PPID
find_sessions_by_ppid() {
    local ppid="$1"
    local base_dir="${2:-.flowloom}"
    local format="${3:-json}"  # json or table
    
    local index_file
    index_file=$(get_session_index_file "$base_dir")
    
    if [[ ! -f "$index_file" ]]; then
        session_warn "Session index not found: $index_file"
        return 1
    fi
    
    python3 << EOF
import json
import sys
from datetime import datetime

try:
    # Read current index
    with open('$index_file', 'r') as f:
        index = json.load(f)
    
    # Find sessions with matching PPID
    matching_sessions = []
    for session_id, session_data in index["sessions"].items():
        if session_data.get("ppid") == int($ppid):
            matching_sessions.append(session_data)
    
    # Sort by last activity (most recent first)
    matching_sessions.sort(key=lambda x: x.get("lastActivity", ""), reverse=True)
    
    if "$format" == "json":
        print(json.dumps(matching_sessions, indent=2))
    elif "$format" == "table":
        if matching_sessions:
            print("SESSION_ID                          STATUS    LAST_ACTIVITY        BRANCH")
            print("-" * 80)
            for session in matching_sessions:
                session_id = session["sessionId"][:35]
                status = session.get("status", "unknown")[:8]
                last_activity = session.get("lastActivity", "")[:19]
                branch = session.get("gitBranch", "")[:15]
                print(f"{session_id:<35} {status:<8} {last_activity:<19} {branch}")
        else:
            print("No sessions found for PPID: $ppid")
    else:
        # Just session IDs
        for session in matching_sessions:
            print(session["sessionId"])
    
except Exception as e:
    print(f"Error finding sessions by PPID: {e}", file=sys.stderr)
    sys.exit(1)
EOF
}

# Get session info from index
get_session_info() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    
    local index_file
    index_file=$(get_session_index_file "$base_dir")
    
    if [[ ! -f "$index_file" ]]; then
        session_error "Session index not found: $index_file"
        return 1
    fi
    
    python3 << EOF
import json
import sys

try:
    # Read current index
    with open('$index_file', 'r') as f:
        index = json.load(f)
    
    # Get session info
    if "$session_id" in index["sessions"]:
        session_info = index["sessions"]["$session_id"]
        print(json.dumps(session_info, indent=2))
    else:
        print("Session not found in index: $session_id", file=sys.stderr)
        sys.exit(1)
    
except Exception as e:
    print(f"Error getting session info: {e}", file=sys.stderr)
    sys.exit(1)
EOF
}

# List all sessions
list_all_sessions() {
    local base_dir="${1:-.flowloom}"
    local status_filter="${2:-all}"  # all, active, inactive, terminated
    local format="${3:-table}"       # table, json, ids
    
    local index_file
    index_file=$(get_session_index_file "$base_dir")
    
    if [[ ! -f "$index_file" ]]; then
        session_warn "Session index not found: $index_file"
        return 1
    fi
    
    python3 << EOF
import json
import sys
from datetime import datetime

try:
    # Read current index
    with open('$index_file', 'r') as f:
        index = json.load(f)
    
    # Filter sessions by status
    sessions = []
    for session_id, session_data in index["sessions"].items():
        if "$status_filter" == "all" or session_data.get("status") == "$status_filter":
            sessions.append(session_data)
    
    # Sort by last activity (most recent first)
    sessions.sort(key=lambda x: x.get("lastActivity", ""), reverse=True)
    
    if "$format" == "json":
        print(json.dumps(sessions, indent=2))
    elif "$format" == "table":
        if sessions:
            print("SESSION_ID                          PPID     STATUS    LAST_ACTIVITY        BRANCH")
            print("-" * 90)
            for session in sessions:
                session_id = session["sessionId"][:35]
                ppid = str(session.get("ppid", 0))[:8]
                status = session.get("status", "unknown")[:8]
                last_activity = session.get("lastActivity", "")[:19]
                branch = session.get("gitBranch", "")[:15]
                print(f"{session_id:<35} {ppid:<8} {status:<8} {last_activity:<19} {branch}")
            print(f"\nTotal: {len(sessions)} sessions")
        else:
            print("No sessions found")
    else:  # ids format
        for session in sessions:
            print(session["sessionId"])
    
except Exception as e:
    print(f"Error listing sessions: {e}", file=sys.stderr)
    sys.exit(1)
EOF
}

# Get index statistics
get_index_stats() {
    local base_dir="${1:-.flowloom}"
    
    local index_file
    index_file=$(get_session_index_file "$base_dir")
    
    if [[ ! -f "$index_file" ]]; then
        session_warn "Session index not found: $index_file"
        return 1
    fi
    
    python3 << EOF
import json
import sys
from collections import Counter

try:
    # Read current index
    with open('$index_file', 'r') as f:
        index = json.load(f)
    
    total_sessions = len(index["sessions"])
    status_counts = Counter(session.get("status", "unknown") for session in index["sessions"].values())
    
    print("FlowLoom Session Index Statistics")
    print("=" * 40)
    print(f"Total Sessions: {total_sessions}")
    print(f"Active Sessions: {status_counts.get('active', 0)}")
    print(f"Inactive Sessions: {status_counts.get('inactive', 0)}")
    print(f"Terminated Sessions: {status_counts.get('terminated', 0)}")
    print(f"Last Updated: {index.get('lastUpdated', 'Unknown')}")
    print(f"Index Version: {index.get('version', 'Unknown')}")
    
    if total_sessions > 0:
        print("\nStatus Distribution:")
        for status, count in status_counts.most_common():
            percentage = (count / total_sessions) * 100
            print(f"  {status}: {count} ({percentage:.1f}%)")
    
except Exception as e:
    print(f"Error getting index stats: {e}", file=sys.stderr)
    sys.exit(1)
EOF
}

# Cleanup index (remove orphaned entries)
cleanup_index() {
    local base_dir="${1:-.flowloom}"
    local dry_run="${2:-false}"
    
    local index_file
    index_file=$(get_session_index_file "$base_dir")
    
    if [[ ! -f "$index_file" ]]; then
        session_warn "Session index not found: $index_file"
        return 1
    fi
    
    session_log "Cleaning up session index (dry_run: $dry_run)"
    
    python3 << EOF
import json
import sys
import os
from datetime import datetime

try:
    # Read current index
    with open('$index_file', 'r') as f:
        index = json.load(f)
    
    sessions_dir = "$base_dir/sessions"
    orphaned_sessions = []
    
    # Check each session in index
    for session_id, session_data in index["sessions"].items():
        session_dir = os.path.join(sessions_dir, session_id)
        if not os.path.exists(session_dir):
            orphaned_sessions.append(session_id)
    
    if orphaned_sessions:
        print(f"Found {len(orphaned_sessions)} orphaned index entries")
        
        if "$dry_run" == "true":
            print("Would remove the following sessions from index:")
            for session_id in orphaned_sessions:
                print(f"  - {session_id}")
        else:
            print("Removing orphaned sessions from index:")
            for session_id in orphaned_sessions:
                print(f"  - {session_id}")
                del index["sessions"][session_id]
            
            # Update counters
            index["totalSessions"] = len(index["sessions"])
            index["activeSessions"] = sum(1 for s in index["sessions"].values() if s.get("status") == "active")
            index["lastUpdated"] = datetime.now().isoformat()
            
            # Write back
            with open('$index_file', 'w') as f:
                json.dump(index, f, indent=2)
            
            print(f"Cleaned up {len(orphaned_sessions)} orphaned entries")
    else:
        print("No orphaned index entries found")
    
except Exception as e:
    print(f"Error cleaning up index: {e}", file=sys.stderr)
    sys.exit(1)
EOF
}

# Main execution for testing
if [[ "${BASH_SOURCE[0]:-$0}" == "${0}" ]]; then
    case "${1:-}" in
        "init")
            initialize_session_index "${2:-.flowloom}"
            ;;
        "add")
            if [[ -z "${2:-}" ]] || [[ -z "${3:-}" ]]; then
                echo "Usage: $0 add <session_id> <metadata_file> [base_dir]" >&2
                exit 1
            fi
            add_session_to_index "$2" "$3" "${4:-.flowloom}"
            ;;
        "remove")
            if [[ -z "${2:-}" ]]; then
                echo "Usage: $0 remove <session_id> [base_dir]" >&2
                exit 1
            fi
            remove_session_from_index "$2" "${3:-.flowloom}"
            ;;
        "update-status")
            if [[ -z "${2:-}" ]] || [[ -z "${3:-}" ]]; then
                echo "Usage: $0 update-status <session_id> <new_status> [base_dir]" >&2
                exit 1
            fi
            update_session_status "$2" "$3" "${4:-.flowloom}"
            ;;
        "find-by-ppid")
            if [[ -z "${2:-}" ]]; then
                echo "Usage: $0 find-by-ppid <ppid> [base_dir] [format]" >&2
                exit 1
            fi
            find_sessions_by_ppid "$2" "${3:-.flowloom}" "${4:-table}"
            ;;
        "get")
            if [[ -z "${2:-}" ]]; then
                echo "Usage: $0 get <session_id> [base_dir]" >&2
                exit 1
            fi
            get_session_info "$2" "${3:-.flowloom}"
            ;;
        "list")
            list_all_sessions "${2:-.flowloom}" "${3:-all}" "${4:-table}"
            ;;
        "stats")
            get_index_stats "${2:-.flowloom}"
            ;;
        "cleanup")
            cleanup_index "${2:-.flowloom}" "${3:-false}"
            ;;
        "test")
            echo "Testing session index functions..."
            test_dir="/tmp/test_flowloom_index_$$"
            echo "Using test directory: $test_dir"
            
            # Initialize index
            echo "Initializing session index..."
            initialize_session_index "$test_dir"
            
            # Create test metadata
            test_session_id="session_12345_$(date +%s)_test01"
            test_metadata="$test_dir/test_metadata.json"
            cat > "$test_metadata" << EOF
{
  "sessionId": "$test_session_id",
  "ppid": 12345,
  "startTime": "$(date -Iseconds)",
  "status": "active",
  "workingDirectory": "/test/dir",
  "gitBranch": "main",
  "lastActivity": "$(date -Iseconds)"
}
EOF
            
            # Test adding session
            echo "Adding test session to index..."
            add_session_to_index "$test_session_id" "$test_metadata" "$test_dir"
            
            # Test listing
            echo "Listing sessions:"
            list_all_sessions "$test_dir" "all" "table"
            
            # Test stats
            echo "Getting index stats:"
            get_index_stats "$test_dir"
            
            # Cleanup
            rm -rf "$test_dir"
            echo "Session index test completed successfully"
            ;;
        *)
            echo "FlowLoom Session Index Management"
            echo "Usage: $0 {init|add|remove|update-status|find-by-ppid|get|list|stats|cleanup|test} [args]"
            echo ""
            echo "Commands:"
            echo "  init [base_dir]                           - Initialize session index"
            echo "  add <session_id> <metadata_file> [base]   - Add session to index"
            echo "  remove <session_id> [base_dir]            - Remove session from index"
            echo "  update-status <session_id> <status> [base] - Update session status"
            echo "  find-by-ppid <ppid> [base] [format]       - Find sessions by PPID"
            echo "  get <session_id> [base_dir]               - Get session info"
            echo "  list [base] [status] [format]             - List sessions"
            echo "  stats [base_dir]                          - Show index statistics"
            echo "  cleanup [base_dir] [dry_run]              - Clean up orphaned entries"
            echo "  test                                      - Run basic functionality test"
            exit 1
            ;;
    esac
fi