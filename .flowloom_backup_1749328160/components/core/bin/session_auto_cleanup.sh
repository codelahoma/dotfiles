#!/bin/bash
# bin/session_auto_cleanup.sh - Automated dead session cleanup
# Part of FlowLoom Session Management System
# Based on Plan 3120 - Session Tracking Detailed Design
#
# SECURITY & ETHICS NOTICE:
# This script provides automated cleanup of dead sessions while respecting 
# Claude Code's security boundaries. Process validation is used only for 
# legitimate session tracking and cleanup operations.

set -euo pipefail

# Source session utilities
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
source "$SCRIPT_DIR/session_utils.sh"

# Auto-cleanup configuration
readonly CLEANUP_MODE="${CLEANUP_MODE:-safe}"  # safe, aggressive
readonly DRY_RUN="${DRY_RUN:-false}"
readonly DEFAULT_BASE_DIR="${FLOWLOOM_WORK_DIR:-.flowloom}"

# Find and cleanup dead sessions
cleanup_dead_sessions() {
    local base_dir="${1:-$DEFAULT_BASE_DIR}"
    local dry_run="${2:-$DRY_RUN}"
    
    session_log "Starting automated dead session cleanup (dry_run: $dry_run)"
    
    local cleaned_count=0
    local preserved_count=0
    local error_count=0
    
    # Get all session worktrees
    while IFS= read -r worktree_line; do
        # Parse worktree list output: /path/to/worktree commit [branch]
        local worktree_path=$(echo "$worktree_line" | awk '{print $1}')
        local branch_name=$(echo "$worktree_line" | sed 's/.*\[\(.*\)\]/\1/')
        
        # Skip if not a session worktree
        if [[ ! "$branch_name" =~ ^session/ ]] && [[ ! "$branch_name" =~ session-[0-9]+ ]]; then
            continue
        fi
        
        # Extract PPID from branch name
        local session_ppid=""
        if [[ "$branch_name" =~ ^session/([0-9]+)- ]]; then
            session_ppid="${BASH_REMATCH[1]}"
        elif [[ "$branch_name" =~ session-([0-9]+) ]]; then
            session_ppid="${BASH_REMATCH[1]}"
        elif [[ "$worktree_path" =~ session-([0-9]+)- ]]; then
            session_ppid="${BASH_REMATCH[1]}"
        fi
        
        if [[ -z "$session_ppid" ]]; then
            session_log "Could not extract PPID from: $worktree_path ($branch_name)"
            continue
        fi
        
        # Check if process is still alive
        if ps -p "$session_ppid" >/dev/null 2>&1; then
            session_log "Session PPID $session_ppid is active, preserving: $worktree_path"
            ((preserved_count++))
            continue
        fi
        
        # Process is dead - clean up session
        session_log "Found dead session PPID $session_ppid: $worktree_path"
        
        if [[ "$dry_run" == "true" ]]; then
            session_log "DRY RUN: Would remove worktree $worktree_path and branch $branch_name"
            ((cleaned_count++))
        else
            session_log "Removing dead session: $worktree_path ($branch_name)"
            
            # Remove worktree and branch
            if git worktree remove "$worktree_path" --force 2>/dev/null; then
                session_log "Successfully removed worktree: $worktree_path"
                
                if git branch -d "$branch_name" 2>/dev/null; then
                    session_log "Successfully removed branch: $branch_name"
                    ((cleaned_count++))
                else
                    session_log "Warning: Failed to remove branch: $branch_name"
                    ((error_count++))
                fi
            else
                session_error "Failed to remove worktree: $worktree_path"
                ((error_count++))
            fi
        fi
        
    done < <(git worktree list 2>/dev/null || true)
    
    # Cleanup summary
    local summary_msg="Auto-cleanup completed: $cleaned_count removed, $preserved_count preserved"
    if [[ "$error_count" -gt 0 ]]; then
        summary_msg="$summary_msg, $error_count errors"
    fi
    session_log "$summary_msg"
    
    # Output JSON result
    cat << EOF
{
  "success": true,
  "cleanupMode": "$CLEANUP_MODE",
  "dryRun": $(if [[ "$dry_run" == "true" ]]; then echo "true"; else echo "false"; fi),
  "sessionsRemoved": $cleaned_count,
  "sessionsPreserved": $preserved_count,
  "errors": $error_count,
  "summary": "$summary_msg"
}
EOF
}

# List dead sessions without cleaning
list_dead_sessions() {
    local base_dir="${1:-$DEFAULT_BASE_DIR}"
    
    session_log "Scanning for dead sessions"
    
    local dead_sessions=()
    local active_sessions=()
    
    # Get all session worktrees
    while IFS= read -r worktree_line; do
        local worktree_path=$(echo "$worktree_line" | awk '{print $1}')
        local branch_name=$(echo "$worktree_line" | sed 's/.*\[\(.*\)\]/\1/')
        
        # Skip if not a session worktree
        if [[ ! "$branch_name" =~ ^session/ ]] && [[ ! "$branch_name" =~ session-[0-9]+ ]]; then
            continue
        fi
        
        # Extract PPID from branch name or path
        local session_ppid=""
        if [[ "$branch_name" =~ ^session/([0-9]+)- ]]; then
            session_ppid="${BASH_REMATCH[1]}"
        elif [[ "$branch_name" =~ session-([0-9]+) ]]; then
            session_ppid="${BASH_REMATCH[1]}"
        elif [[ "$worktree_path" =~ session-([0-9]+)- ]]; then
            session_ppid="${BASH_REMATCH[1]}"
        fi
        
        if [[ -z "$session_ppid" ]]; then
            continue
        fi
        
        # Check if process is alive
        if ps -p "$session_ppid" >/dev/null 2>&1; then
            active_sessions+=("{\"ppid\":$session_ppid,\"path\":\"$worktree_path\",\"branch\":\"$branch_name\",\"status\":\"active\"}")
        else
            dead_sessions+=("{\"ppid\":$session_ppid,\"path\":\"$worktree_path\",\"branch\":\"$branch_name\",\"status\":\"dead\"}")
        fi
        
    done < <(git worktree list 2>/dev/null || true)
    
    # Output JSON result
    cat << EOF
{
  "success": true,
  "deadSessions": [$(IFS=,; echo "${dead_sessions[*]}")],
  "activeSessions": [$(IFS=,; echo "${active_sessions[*]}")],
  "deadCount": ${#dead_sessions[@]},
  "activeCount": ${#active_sessions[@]}
}
EOF
}

# Print help
print_auto_cleanup_help() {
    cat << EOF
FlowLoom Automated Session Cleanup

Usage: session_auto_cleanup.sh <command> [options]

Commands:
  cleanup [base_dir] [dry_run]    Clean up dead sessions
  list [base_dir]                 List dead and active sessions
  help                           Show this help

Parameters:
  base_dir: Session base directory (default: \${FLOWLOOM_WORK_DIR:-.flowloom})
  dry_run: true/false - Preview cleanup without making changes (default: false)

Environment Variables:
  CLEANUP_MODE: safe, aggressive (default: safe)
  DRY_RUN: true/false (default: false)
  FLOWLOOM_WORK_DIR: Base directory for session data

Examples:
  session_auto_cleanup.sh list
  session_auto_cleanup.sh cleanup . true    # Dry run
  session_auto_cleanup.sh cleanup . false   # Actually clean up
  DRY_RUN=true session_auto_cleanup.sh cleanup

Security:
  - Only checks process existence for PPID validation
  - Respects Claude Code security boundaries
  - No unauthorized process inspection or manipulation
  - Local file operations only
EOF
}

# Main command dispatcher
main() {
    if [[ $# -eq 0 ]]; then
        print_auto_cleanup_help
        exit 1
    fi
    
    local command="$1"
    shift
    
    case "$command" in
        "cleanup")
            cleanup_dead_sessions "$@"
            ;;
        "list")
            list_dead_sessions "$@"
            ;;
        "help"|"--help"|"-h")
            print_auto_cleanup_help
            ;;
        *)
            echo "Error: Unknown command: $command" >&2
            echo "Use 'session_auto_cleanup.sh help' for usage information" >&2
            exit 1
            ;;
    esac
}

# Run main function if script is executed directly
if [[ "${BASH_SOURCE[0]:-$0}" == "${0}" ]]; then
    main "$@"
fi