#!/bin/bash
# bin/session_backup.sh - Session backup and recovery mechanisms
# Part of FlowLoom Session Management System - Phase 2: State Management
# Based on Plan 3120 - Session Tracking Detailed Design
#
# SECURITY & ETHICS NOTICE:
# This script manages session backup and recovery while respecting Claude Code's
# security boundaries. All operations are confined to designated session directories
# and do not access unauthorized resources or attempt to circumvent security measures.

set -euo pipefail

# Import session utilities
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
source "$SCRIPT_DIR/session_utils.sh"
source "$SCRIPT_DIR/session_filesystem.sh"

# Backup configuration
readonly BACKUP_RETENTION_DAYS=30
readonly MAX_FULL_BACKUPS=10
readonly BACKUP_COMPRESSION="gzip"

# Session backup functions

# Create full session backup
create_full_session_backup() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    local backup_note="${3:-Manual backup}"
    
    session_debug "Creating full backup for session: $session_id"
    
    if ! validate_session_id "$session_id"; then
        session_error "Invalid session ID: $session_id"
        return 1
    fi
    
    local session_dir
    session_dir=$(get_session_directory "$session_id" "$base_dir")
    
    if [[ ! -d "$session_dir" ]]; then
        session_error "Session directory not found: $session_dir"
        return 1
    fi
    
    # Create backup directory structure
    local backup_base_dir="$base_dir/backups"
    local backup_timestamp
    backup_timestamp=$(date +%Y%m%d_%H%M%S)
    local backup_dir="$backup_base_dir/$session_id"
    
    mkdir -p "$backup_dir"
    
    # Create backup manifest
    local manifest_file="$backup_dir/backup_${backup_timestamp}.manifest"
    
    cat > "$manifest_file" << EOF
{
  "backupId": "${session_id}_${backup_timestamp}",
  "sessionId": "$session_id",
  "timestamp": "$(date -Iseconds)",
  "type": "full",
  "note": "$backup_note",
  "files": []
}
EOF
    
    # Backup all session files
    local backup_archive="$backup_dir/session_${backup_timestamp}.tar.gz"
    local files_backed_up=0
    
    cd "$session_dir"
    
    # Create file list for manifest
    local file_list=()
    while IFS= read -r -d '' file; do
        local relative_path="${file#./}"
        if [[ "$relative_path" != "." ]]; then
            file_list+=("\"$relative_path\"")
            ((files_backed_up++))
        fi
    done < <(find . -type f -print0)
    
    # Update manifest with file list
    python3 << EOF
import json

with open('$manifest_file', 'r') as f:
    manifest = json.load(f)

manifest['files'] = [$(IFS=,; echo "${file_list[*]}")]
manifest['fileCount'] = $files_backed_up

with open('$manifest_file', 'w') as f:
    json.dump(manifest, f, indent=2)
EOF
    
    # Create compressed archive
    if tar -czf "$backup_archive" .; then
        local backup_size
        backup_size=$(stat -c%s "$backup_archive")
        
        # Update manifest with backup info
        python3 << EOF
import json

with open('$manifest_file', 'r') as f:
    manifest = json.load(f)

manifest['archiveFile'] = 'session_${backup_timestamp}.tar.gz'
manifest['archiveSize'] = $backup_size
manifest['status'] = 'completed'

with open('$manifest_file', 'w') as f:
    json.dump(manifest, f, indent=2)
EOF
        
        session_log "Created full backup: $backup_archive ($files_backed_up files, $backup_size bytes)"
        echo "$backup_archive"
    else
        session_error "Failed to create backup archive: $backup_archive"
        rm -f "$backup_archive" "$manifest_file"
        return 1
    fi
    
    # Cleanup old backups
    cleanup_old_backups "$session_id" "$base_dir"
}

# Create incremental backup (state and metadata only)
create_incremental_backup() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    local backup_note="${3:-Incremental backup}"
    
    session_debug "Creating incremental backup for session: $session_id"
    
    local session_dir
    session_dir=$(get_session_directory "$session_id" "$base_dir")
    
    local backup_base_dir="$base_dir/backups"
    local backup_timestamp
    backup_timestamp=$(date +%Y%m%d_%H%M%S)
    local backup_dir="$backup_base_dir/$session_id"
    
    mkdir -p "$backup_dir"
    
    # Backup critical files only
    local incremental_dir="$backup_dir/incremental_${backup_timestamp}"
    mkdir -p "$incremental_dir"
    
    local files_backed_up=0
    
    # Copy state file
    if [[ -f "$session_dir/state.json" ]]; then
        cp "$session_dir/state.json" "$incremental_dir/"
        ((files_backed_up++))
    fi
    
    # Copy metadata file
    if [[ -f "$session_dir/metadata.json" ]]; then
        cp "$session_dir/metadata.json" "$incremental_dir/"
        ((files_backed_up++))
    fi
    
    # Create manifest
    local manifest_file="$incremental_dir/backup.manifest"
    
    cat > "$manifest_file" << EOF
{
  "backupId": "${session_id}_${backup_timestamp}_incremental",
  "sessionId": "$session_id",
  "timestamp": "$(date -Iseconds)",
  "type": "incremental",
  "note": "$backup_note",
  "fileCount": $files_backed_up,
  "status": "completed"
}
EOF
    
    session_log "Created incremental backup: $incremental_dir ($files_backed_up files)"
    echo "$incremental_dir"
}

# Restore session from backup
restore_session_from_backup() {
    local session_id="$1"
    local backup_id="${2:-latest}"
    local base_dir="${3:-.flowloom}"
    local restore_mode="${4:-full}"  # full, state-only, metadata-only
    
    session_debug "Restoring session $session_id from backup: $backup_id (mode: $restore_mode)"
    
    local backup_base_dir="$base_dir/backups"
    local backup_dir="$backup_base_dir/$session_id"
    
    if [[ ! -d "$backup_dir" ]]; then
        session_error "No backups found for session: $session_id"
        return 1
    fi
    
    local backup_file=""
    local manifest_file=""
    
    if [[ "$backup_id" == "latest" ]]; then
        # Find latest backup
        local latest_manifest
        latest_manifest=$(find "$backup_dir" -name "backup_*.manifest" -type f -printf '%T+ %p\n' | sort -r | head -n1 | cut -d' ' -f2-)
        
        if [[ -z "$latest_manifest" ]]; then
            session_error "No backup manifests found"
            return 1
        fi
        
        manifest_file="$latest_manifest"
        
        # Get backup type and file
        local backup_type
        backup_type=$(get_json_field "$manifest_file" "type" "unknown")
        
        if [[ "$backup_type" == "full" ]]; then
            local archive_name
            archive_name=$(get_json_field "$manifest_file" "archiveFile" "")
            backup_file="$backup_dir/$archive_name"
        else
            backup_file="$(dirname "$manifest_file")"
        fi
    else
        # Specific backup ID
        if [[ -f "$backup_dir/backup_${backup_id}.manifest" ]]; then
            manifest_file="$backup_dir/backup_${backup_id}.manifest"
            local archive_name
            archive_name=$(get_json_field "$manifest_file" "archiveFile" "")
            backup_file="$backup_dir/$archive_name"
        elif [[ -d "$backup_dir/incremental_${backup_id}" ]]; then
            backup_file="$backup_dir/incremental_${backup_id}"
            manifest_file="$backup_file/backup.manifest"
        else
            session_error "Backup not found: $backup_id"
            return 1
        fi
    fi
    
    if [[ ! -f "$manifest_file" ]]; then
        session_error "Backup manifest not found: $manifest_file"
        return 1
    fi
    
    # Validate backup before restoring
    if ! validate_json_file "$manifest_file"; then
        session_error "Backup manifest is corrupted: $manifest_file"
        return 1
    fi
    
    local session_dir
    session_dir=$(get_session_directory "$session_id" "$base_dir")
    
    # Create backup of current session before restoring
    if [[ -d "$session_dir" ]]; then
        local pre_restore_backup
        pre_restore_backup=$(create_full_session_backup "$session_id" "$base_dir" "Pre-restore backup" 2>/dev/null)
        session_log "Created pre-restore backup: $pre_restore_backup"
    fi
    
    # Ensure session directory exists
    mkdir -p "$session_dir"
    
    local backup_type
    backup_type=$(get_json_field "$manifest_file" "type" "unknown")
    
    case "$backup_type" in
        "full")
            if [[ "$restore_mode" == "full" ]]; then
                # Extract full backup
                cd "$session_dir"
                if tar -xzf "$backup_file"; then
                    session_log "Restored full session from: $backup_file"
                else
                    session_error "Failed to extract backup: $backup_file"
                    return 1
                fi
            else
                # Extract specific files from full backup
                local temp_dir
                temp_dir=$(mktemp -d)
                cd "$temp_dir"
                
                if tar -xzf "$backup_file"; then
                    case "$restore_mode" in
                        "state-only")
                            if [[ -f "state.json" ]]; then
                                cp "state.json" "$session_dir/"
                                session_log "Restored state.json from: $backup_file"
                            fi
                            ;;
                        "metadata-only")
                            if [[ -f "metadata.json" ]]; then
                                cp "metadata.json" "$session_dir/"
                                session_log "Restored metadata.json from: $backup_file"
                            fi
                            ;;
                    esac
                else
                    session_error "Failed to extract backup for partial restore: $backup_file"
                    rm -rf "$temp_dir"
                    return 1
                fi
                
                rm -rf "$temp_dir"
            fi
            ;;
        "incremental")
            case "$restore_mode" in
                "full")
                    if [[ -f "$backup_file/state.json" ]]; then
                        cp "$backup_file/state.json" "$session_dir/"
                        session_log "Restored state.json from incremental backup"
                    fi
                    if [[ -f "$backup_file/metadata.json" ]]; then
                        cp "$backup_file/metadata.json" "$session_dir/"
                        session_log "Restored metadata.json from incremental backup"
                    fi
                    ;;
                "state-only")
                    if [[ -f "$backup_file/state.json" ]]; then
                        cp "$backup_file/state.json" "$session_dir/"
                        session_log "Restored state.json from incremental backup"
                    fi
                    ;;
                "metadata-only")
                    if [[ -f "$backup_file/metadata.json" ]]; then
                        cp "$backup_file/metadata.json" "$session_dir/"
                        session_log "Restored metadata.json from incremental backup"
                    fi
                    ;;
            esac
            ;;
        *)
            session_error "Unknown backup type: $backup_type"
            return 1
            ;;
    esac
    
    echo "$session_dir"
}

# List available backups for a session
list_session_backups() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    local output_format="${3:-table}"  # table, json, ids
    
    local backup_base_dir="$base_dir/backups"
    local backup_dir="$backup_base_dir/$session_id"
    
    if [[ ! -d "$backup_dir" ]]; then
        case "$output_format" in
            "json") echo "[]" ;;
            "ids") ;;
            *) echo "No backups found for session: $session_id" ;;
        esac
        return 0
    fi
    
    local backups=()
    
    # Find all backup manifests
    while IFS= read -r -d '' manifest_file; do
        if validate_json_file "$manifest_file"; then
            local backup_id
            backup_id=$(get_json_field "$manifest_file" "backupId" "unknown")
            local timestamp
            timestamp=$(get_json_field "$manifest_file" "timestamp" "unknown")
            local backup_type
            backup_type=$(get_json_field "$manifest_file" "type" "unknown")
            local note
            note=$(get_json_field "$manifest_file" "note" "")
            
            case "$output_format" in
                "json")
                    backups+=("{\"id\":\"$backup_id\",\"timestamp\":\"$timestamp\",\"type\":\"$backup_type\",\"note\":\"$note\"}")
                    ;;
                "ids")
                    backups+=("$backup_id")
                    ;;
                *)
                    local size=""
                    if [[ "$backup_type" == "full" ]]; then
                        local archive_name
                        archive_name=$(get_json_field "$manifest_file" "archiveFile" "")
                        if [[ -f "$backup_dir/$archive_name" ]]; then
                            size=$(stat -c%s "$backup_dir/$archive_name" | numfmt --to=iec-i --suffix=B)
                        fi
                    fi
                    backups+=("$backup_id|$timestamp|$backup_type|$size|$note")
                    ;;
            esac
        fi
    done < <(find "$backup_dir" -name "backup*.manifest" -type f -print0)
    
    case "$output_format" in
        "json")
            echo "[$(IFS=,; echo "${backups[*]}")]"
            ;;
        "ids")
            printf '%s\n' "${backups[@]}"
            ;;
        *)
            if [[ ${#backups[@]} -eq 0 ]]; then
                echo "No valid backups found for session: $session_id"
            else
                echo "Backup ID|Timestamp|Type|Size|Note"
                echo "---------|---------|----|----|----"
                printf '%s\n' "${backups[@]}" | sort -r | column -t -s'|'
            fi
            ;;
    esac
}

# Cleanup old backups
cleanup_old_backups() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    local dry_run="${3:-false}"
    
    session_debug "Cleaning up old backups for session: $session_id (dry_run: $dry_run)"
    
    local backup_base_dir="$base_dir/backups"
    local backup_dir="$backup_base_dir/$session_id"
    
    if [[ ! -d "$backup_dir" ]]; then
        return 0
    fi
    
    local cutoff_date
    cutoff_date=$(date -d "-${BACKUP_RETENTION_DAYS} days" +%s)
    
    local cleaned_count=0
    local total_size_freed=0
    
    # Find old backup manifests
    while IFS= read -r -d '' manifest_file; do
        local backup_timestamp
        backup_timestamp=$(get_json_field "$manifest_file" "timestamp" "")
        
        if [[ -n "$backup_timestamp" ]]; then
            local backup_date
            backup_date=$(date -d "$backup_timestamp" +%s 2>/dev/null || echo "0")
            
            if [[ $backup_date -lt $cutoff_date ]]; then
                local backup_type
                backup_type=$(get_json_field "$manifest_file" "type" "unknown")
                
                local files_to_remove=("$manifest_file")
                local size_freed=0
                
                if [[ "$backup_type" == "full" ]]; then
                    local archive_name
                    archive_name=$(get_json_field "$manifest_file" "archiveFile" "")
                    local archive_file="$backup_dir/$archive_name"
                    
                    if [[ -f "$archive_file" ]]; then
                        size_freed=$(stat -c%s "$archive_file")
                        files_to_remove+=("$archive_file")
                    fi
                else
                    # Incremental backup directory
                    local backup_subdir
                    backup_subdir=$(dirname "$manifest_file")
                    if [[ -d "$backup_subdir" && "$backup_subdir" != "$backup_dir" ]]; then
                        size_freed=$(du -sb "$backup_subdir" | cut -f1)
                        files_to_remove=("$backup_subdir")
                    fi
                fi
                
                if [[ "$dry_run" == "true" ]]; then
                    session_log "Would remove backup: $manifest_file ($(numfmt --to=iec-i --suffix=B <<< $size_freed))"
                else
                    rm -rf "${files_to_remove[@]}"
                    session_log "Removed old backup: $manifest_file ($(numfmt --to=iec-i --suffix=B <<< $size_freed))"
                fi
                
                ((cleaned_count++))
                ((total_size_freed += size_freed))
            fi
        fi
    done < <(find "$backup_dir" -name "backup*.manifest" -type f -print0)
    
    # Limit number of full backups
    local full_backup_count
    full_backup_count=$(find "$backup_dir" -name "backup_*.manifest" -type f -exec grep -l '"type": "full"' {} \; | wc -l)
    
    if [[ $full_backup_count -gt $MAX_FULL_BACKUPS ]]; then
        local excess_count=$((full_backup_count - MAX_FULL_BACKUPS))
        
        # Remove oldest full backups
        find "$backup_dir" -name "backup_*.manifest" -type f -exec grep -l '"type": "full"' {} \; -printf '%T+ %p\n' | sort | head -n $excess_count | while read -r timestamp manifest_file; do
            local archive_name
            archive_name=$(get_json_field "$manifest_file" "archiveFile" "")
            local archive_file="$backup_dir/$archive_name"
            
            local size_freed=0
            if [[ -f "$archive_file" ]]; then
                size_freed=$(stat -c%s "$archive_file")
            fi
            
            if [[ "$dry_run" == "true" ]]; then
                session_log "Would remove excess backup: $manifest_file ($(numfmt --to=iec-i --suffix=B <<< $size_freed))"
            else
                rm -f "$manifest_file" "$archive_file"
                session_log "Removed excess backup: $manifest_file ($(numfmt --to=iec-i --suffix=B <<< $size_freed))"
            fi
            
            ((cleaned_count++))
            ((total_size_freed += size_freed))
        done
    fi
    
    if [[ $cleaned_count -gt 0 ]]; then
        session_log "Cleanup completed: $cleaned_count backups removed, $(numfmt --to=iec-i --suffix=B <<< $total_size_freed) freed"
    fi
    
    echo "$cleaned_count"
}

# Verify backup integrity
verify_backup_integrity() {
    local session_id="$1"
    local backup_id="${2:-all}"
    local base_dir="${3:-.flowloom}"
    
    session_debug "Verifying backup integrity for session: $session_id (backup: $backup_id)"
    
    local backup_base_dir="$base_dir/backups"
    local backup_dir="$backup_base_dir/$session_id"
    
    if [[ ! -d "$backup_dir" ]]; then
        session_error "No backups found for session: $session_id"
        return 1
    fi
    
    local verification_errors=0
    local verified_count=0
    
    # Find backup manifests to verify
    local manifest_pattern="backup*.manifest"
    if [[ "$backup_id" != "all" ]]; then
        manifest_pattern="backup_${backup_id}.manifest"
    fi
    
    while IFS= read -r -d '' manifest_file; do
        session_debug "Verifying backup: $manifest_file"
        
        # Verify manifest JSON
        if ! validate_json_file "$manifest_file"; then
            session_error "Corrupted manifest: $manifest_file"
            ((verification_errors++))
            continue
        fi
        
        local backup_type
        backup_type=$(get_json_field "$manifest_file" "type" "unknown")
        
        case "$backup_type" in
            "full")
                local archive_name
                archive_name=$(get_json_field "$manifest_file" "archiveFile" "")
                local archive_file="$backup_dir/$archive_name"
                
                if [[ ! -f "$archive_file" ]]; then
                    session_error "Missing archive file: $archive_file"
                    ((verification_errors++))
                else
                    # Test archive integrity
                    if tar -tzf "$archive_file" >/dev/null 2>&1; then
                        session_debug "Archive integrity OK: $archive_file"
                    else
                        session_error "Corrupted archive: $archive_file"
                        ((verification_errors++))
                    fi
                fi
                ;;
            "incremental")
                local backup_subdir
                backup_subdir=$(dirname "$manifest_file")
                
                # Check if required files exist
                local required_files=("state.json" "metadata.json")
                for file in "${required_files[@]}"; do
                    if [[ -f "$backup_subdir/$file" ]]; then
                        if ! validate_json_file "$backup_subdir/$file"; then
                            session_error "Corrupted file in backup: $backup_subdir/$file"
                            ((verification_errors++))
                        fi
                    fi
                done
                ;;
        esac
        
        ((verified_count++))
    done < <(find "$backup_dir" -name "$manifest_pattern" -type f -print0)
    
    if [[ $verified_count -eq 0 ]]; then
        session_error "No backups found to verify"
        return 1
    fi
    
    if [[ $verification_errors -eq 0 ]]; then
        session_log "Backup verification completed: $verified_count backups verified, no errors"
        echo "ok"
    else
        session_error "Backup verification completed: $verified_count backups verified, $verification_errors errors"
        echo "errors: $verification_errors"
        return 1
    fi
}

# Print help
print_backup_help() {
    cat << EOF
FlowLoom Session Backup and Recovery

Usage: session_backup.sh <command> [options]

Commands:
  create-full <session_id> [base_dir] [note]     Create full session backup
  create-incremental <session_id> [base_dir] [note]  Create incremental backup
  restore <session_id> [backup_id] [base_dir] [mode]  Restore from backup
  list <session_id> [base_dir] [format]         List available backups
  cleanup <session_id> [base_dir] [dry_run]     Cleanup old backups
  verify <session_id> [backup_id] [base_dir]    Verify backup integrity

Parameters:
  backup_id: Specific backup ID or 'latest' (default: latest)
  mode: full, state-only, metadata-only (default: full)
  format: table, json, ids (default: table)
  dry_run: true/false (default: false)

Examples:
  session_backup.sh create-full session_123_456_abc123
  session_backup.sh restore session_123_456_abc123 latest
  session_backup.sh list session_123_456_abc123
  session_backup.sh cleanup session_123_456_abc123 false
  session_backup.sh verify session_123_456_abc123

Notes:
  - Full backups include all session files (compressed)
  - Incremental backups include only state and metadata
  - Automatic cleanup based on retention policy ($BACKUP_RETENTION_DAYS days)
  - Maximum $MAX_FULL_BACKUPS full backups retained per session
  - Respects Claude Code security boundaries
EOF
}

# Main command dispatcher
main() {
    if [[ $# -eq 0 ]]; then
        print_backup_help
        exit 1
    fi
    
    local command="$1"
    shift
    
    case "$command" in
        "create-full")
            if [[ $# -lt 1 ]]; then
                echo "Error: create-full requires session_id" >&2
                exit 1
            fi
            create_full_session_backup "$@"
            ;;
        "create-incremental")
            if [[ $# -lt 1 ]]; then
                echo "Error: create-incremental requires session_id" >&2
                exit 1
            fi
            create_incremental_backup "$@"
            ;;
        "restore")
            if [[ $# -lt 1 ]]; then
                echo "Error: restore requires session_id" >&2
                exit 1
            fi
            restore_session_from_backup "$@"
            ;;
        "list")
            if [[ $# -lt 1 ]]; then
                echo "Error: list requires session_id" >&2
                exit 1
            fi
            list_session_backups "$@"
            ;;
        "cleanup")
            if [[ $# -lt 1 ]]; then
                echo "Error: cleanup requires session_id" >&2
                exit 1
            fi
            cleanup_old_backups "$@"
            ;;
        "verify")
            if [[ $# -lt 1 ]]; then
                echo "Error: verify requires session_id" >&2
                exit 1
            fi
            verify_backup_integrity "$@"
            ;;
        "help"|"--help"|"-h")
            print_backup_help
            ;;
        *)
            echo "Error: Unknown command: $command" >&2
            echo "Use 'session_backup.sh help' for usage information" >&2
            exit 1
            ;;
    esac
}

# Run main function if script is executed directly
if [[ "${BASH_SOURCE[0]:-$0}" == "${0}" ]]; then
    main "$@"
fi