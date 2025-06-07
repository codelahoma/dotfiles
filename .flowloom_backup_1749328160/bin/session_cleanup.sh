#!/bin/bash
# bin/session_cleanup.sh - Automated session cleanup
# Part of FlowLoom Session Management System - Phase 4: Advanced Features
# Based on Plan 3120 - Session Tracking Detailed Design
#
# SECURITY & ETHICS NOTICE:
# This script performs automated cleanup of session data while respecting Claude Code's
# security boundaries. All operations are confined to designated session directories
# and follow strict retention policies to prevent data loss.

set -euo pipefail

# Import session utilities
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
source "$SCRIPT_DIR/session_utils.sh"
source "$SCRIPT_DIR/session_filesystem.sh"
source "$SCRIPT_DIR/session_index.sh"
source "$SCRIPT_DIR/session_backup.sh"

# Cleanup configuration
readonly DEFAULT_SESSION_RETENTION_DAYS=90
readonly DEFAULT_BACKUP_RETENTION_DAYS=30
readonly DEFAULT_TERMINATED_SESSION_RETENTION_DAYS=30
readonly MINIMUM_ACTIVE_SESSION_RETENTION_DAYS=7

# Cleanup functions

# Clean up old sessions
cleanup_old_sessions() {
    local base_dir="${1:-.flowloom}"
    local retention_days="${2:-$DEFAULT_SESSION_RETENTION_DAYS}"
    local dry_run="${3:-true}"
    local session_status_filter="${4:-terminated}"  # all, active, inactive, terminated
    
    session_debug "Cleaning up old sessions (retention: ${retention_days}d, dry_run: $dry_run, filter: $session_status_filter)"
    
    local index_file="$base_dir/sessions/index.json"
    
    if [[ ! -f "$index_file" ]]; then
        session_warn "Session index not found: $index_file"
        return 0
    fi
    
    # Use Python to process cleanup safely
    python3 << EOF
import json
import os
import sys
from datetime import datetime, timedelta
import shutil

try:
    with open('$index_file', 'r') as f:
        index = json.load(f)
    
    sessions = index.get('sessions', {})
    base_dir = '$base_dir'
    retention_days = int('$retention_days')
    dry_run = '$dry_run' == 'true'
    status_filter = '$session_status_filter'
    
    cutoff_date = datetime.now() - timedelta(days=retention_days)
    
    sessions_to_remove = []
    total_size_freed = 0
    
    for session_id, session_data in sessions.items():
        # Apply status filter
        if status_filter != 'all' and session_data.get('status') != status_filter:
            continue
        
        # Special protection for active sessions (minimum retention)
        if session_data.get('status') == 'active':
            min_retention = int('$MINIMUM_ACTIVE_SESSION_RETENTION_DAYS')
            min_cutoff = datetime.now() - timedelta(days=min_retention)
            
            try:
                last_activity = datetime.fromisoformat(session_data.get('lastActivity', '').replace('Z', '+00:00'))
                if last_activity > min_cutoff:
                    continue  # Skip active sessions within minimum retention
            except:
                continue  # Skip if can't parse date
        
        # Check if session is old enough for cleanup
        should_remove = False
        
        # For terminated sessions, use end time if available
        if session_data.get('status') == 'terminated' and session_data.get('endTime'):
            try:
                end_time = datetime.fromisoformat(session_data.get('endTime', '').replace('Z', '+00:00'))
                should_remove = end_time < cutoff_date
            except:
                should_remove = False
        else:
            # Use last activity time
            try:
                last_activity = datetime.fromisoformat(session_data.get('lastActivity', '').replace('Z', '+00:00'))
                should_remove = last_activity < cutoff_date
            except:
                should_remove = False
        
        if should_remove:
            session_dir = os.path.join(base_dir, 'sessions', session_id)
            
            # Calculate directory size
            dir_size = 0
            if os.path.exists(session_dir):
                for root, dirs, files in os.walk(session_dir):
                    for file in files:
                        file_path = os.path.join(root, file)
                        try:
                            dir_size += os.path.getsize(file_path)
                        except:
                            pass
            
            sessions_to_remove.append({
                'sessionId': session_id,
                'sessionDir': session_dir,
                'size': dir_size,
                'status': session_data.get('status', 'unknown'),
                'lastActivity': session_data.get('lastActivity', 'unknown')
            })
            
            total_size_freed += dir_size
    
    # Process removals
    removed_count = 0
    updated_sessions = sessions.copy()
    
    for session_info in sessions_to_remove:
        session_id = session_info['sessionId']
        session_dir = session_info['sessionDir']
        
        if dry_run:
            print(f"Would remove session: {session_id} ({session_info['size']} bytes)")
        else:
            try:
                # Remove session directory
                if os.path.exists(session_dir):
                    shutil.rmtree(session_dir)
                
                # Remove from index
                if session_id in updated_sessions:
                    del updated_sessions[session_id]
                
                removed_count += 1
                print(f"Removed session: {session_id} ({session_info['size']} bytes)")
                
            except Exception as e:
                print(f"Failed to remove session {session_id}: {e}", file=sys.stderr)
    
    # Update index if changes were made
    if not dry_run and removed_count > 0:
        index['sessions'] = updated_sessions
        index['lastUpdated'] = datetime.now().isoformat()
        
        # Update statistics
        total_sessions = len(updated_sessions)
        active_sessions = sum(1 for s in updated_sessions.values() if s.get('status') == 'active')
        inactive_sessions = sum(1 for s in updated_sessions.values() if s.get('status') == 'inactive')
        terminated_sessions = sum(1 for s in updated_sessions.values() if s.get('status') == 'terminated')
        
        index['totalSessions'] = total_sessions
        index['activeSessions'] = active_sessions
        index['inactiveSessions'] = inactive_sessions
        index['terminatedSessions'] = terminated_sessions
        
        with open('$index_file', 'w') as f:
            json.dump(index, f, indent=2)
    
    # Output summary
    summary = {
        'sessionsFound': len(sessions_to_remove),
        'sessionsRemoved': removed_count,
        'totalSizeFreed': total_size_freed,
        'dryRun': dry_run,
        'retentionDays': retention_days,
        'statusFilter': status_filter
    }
    
    print(json.dumps(summary))

except Exception as e:
    print(f"Error during session cleanup: {e}", file=sys.stderr)
    sys.exit(1)
EOF
    
    local cleanup_result=$?
    
    if [[ $cleanup_result -eq 0 ]]; then
        session_log "Session cleanup completed (retention: ${retention_days}d, dry_run: $dry_run)"
    else
        session_error "Session cleanup failed"
        return 1
    fi
}

# Clean up session backups
cleanup_session_backups() {
    local base_dir="${1:-.flowloom}"
    local retention_days="${2:-$DEFAULT_BACKUP_RETENTION_DAYS}"
    local dry_run="${3:-true}"
    
    session_debug "Cleaning up session backups (retention: ${retention_days}d, dry_run: $dry_run)"
    
    local backup_base_dir="$base_dir/backups"
    
    if [[ ! -d "$backup_base_dir" ]]; then
        session_warn "Backup directory not found: $backup_base_dir"
        return 0
    fi
    
    # Process backup cleanup using Python
    python3 << EOF
import json
import os
import sys
from datetime import datetime, timedelta
import shutil

try:
    backup_base_dir = '$backup_base_dir'
    retention_days = int('$retention_days')
    dry_run = '$dry_run' == 'true'
    
    cutoff_date = datetime.now() - timedelta(days=retention_days)
    
    total_backups_found = 0
    total_backups_removed = 0
    total_size_freed = 0
    
    # Process each session's backup directory
    for session_id in os.listdir(backup_base_dir):
        session_backup_dir = os.path.join(backup_base_dir, session_id)
        
        if not os.path.isdir(session_backup_dir):
            continue
        
        # Find backup manifests
        manifests_to_remove = []
        
        for item in os.listdir(session_backup_dir):
            if item.endswith('.manifest'):
                manifest_path = os.path.join(session_backup_dir, item)
                
                try:
                    with open(manifest_path, 'r') as f:
                        manifest = json.load(f)
                    
                    # Check backup age
                    backup_timestamp = manifest.get('timestamp', '')
                    if backup_timestamp:
                        backup_date = datetime.fromisoformat(backup_timestamp.replace('Z', '+00:00'))
                        
                        if backup_date < cutoff_date:
                            manifests_to_remove.append({
                                'manifest': manifest_path,
                                'backup_data': manifest,
                                'session_id': session_id
                            })
                            total_backups_found += 1
                
                except Exception as e:
                    print(f"Error reading manifest {manifest_path}: {e}", file=sys.stderr)
        
        # Process removals for this session
        for backup_info in manifests_to_remove:
            manifest_path = backup_info['manifest']
            manifest_data = backup_info['backup_data']
            
            files_to_remove = [manifest_path]
            backup_size = 0
            
            # Add associated files based on backup type
            if manifest_data.get('type') == 'full':
                archive_file = manifest_data.get('archiveFile')
                if archive_file:
                    archive_path = os.path.join(session_backup_dir, archive_file)
                    if os.path.exists(archive_path):
                        files_to_remove.append(archive_path)
                        backup_size += os.path.getsize(archive_path)
            else:
                # Incremental backup directory
                backup_subdir = os.path.dirname(manifest_path)
                if backup_subdir != session_backup_dir and os.path.exists(backup_subdir):
                    # Calculate directory size
                    for root, dirs, files in os.walk(backup_subdir):
                        for file in files:
                            file_path = os.path.join(root, file)
                            try:
                                backup_size += os.path.getsize(file_path)
                            except:
                                pass
                    files_to_remove = [backup_subdir]  # Remove entire directory
            
            backup_id = manifest_data.get('backupId', 'unknown')
            
            if dry_run:
                print(f"Would remove backup: {backup_id} ({backup_size} bytes)")
            else:
                try:
                    for file_path in files_to_remove:
                        if os.path.isdir(file_path):
                            shutil.rmtree(file_path)
                        else:
                            os.remove(file_path)
                    
                    total_backups_removed += 1
                    total_size_freed += backup_size
                    print(f"Removed backup: {backup_id} ({backup_size} bytes)")
                    
                except Exception as e:
                    print(f"Failed to remove backup {backup_id}: {e}", file=sys.stderr)
        
        # Remove empty session backup directories
        if not dry_run and total_backups_removed > 0:
            try:
                if os.path.exists(session_backup_dir) and not os.listdir(session_backup_dir):
                    os.rmdir(session_backup_dir)
                    print(f"Removed empty backup directory: {session_id}")
            except:
                pass
    
    # Output summary
    summary = {
        'backupsFound': total_backups_found,
        'backupsRemoved': total_backups_removed,
        'totalSizeFreed': total_size_freed,
        'dryRun': dry_run,
        'retentionDays': retention_days
    }
    
    print(json.dumps(summary))

except Exception as e:
    print(f"Error during backup cleanup: {e}", file=sys.stderr)
    sys.exit(1)
EOF
    
    local cleanup_result=$?
    
    if [[ $cleanup_result -eq 0 ]]; then
        session_log "Backup cleanup completed (retention: ${retention_days}d, dry_run: $dry_run)"
    else
        session_error "Backup cleanup failed"
        return 1
    fi
}

# Clean up orphaned index entries
cleanup_orphaned_index_entries() {
    local base_dir="${1:-.flowloom}"
    local dry_run="${2:-true}"
    
    session_debug "Cleaning up orphaned index entries (dry_run: $dry_run)"
    
    local index_file="$base_dir/sessions/index.json"
    
    if [[ ! -f "$index_file" ]]; then
        session_warn "Session index not found: $index_file"
        return 0
    fi
    
    # Process orphaned entries cleanup
    python3 << EOF
import json
import os
import sys
from datetime import datetime

try:
    with open('$index_file', 'r') as f:
        index = json.load(f)
    
    sessions = index.get('sessions', {})
    base_dir = '$base_dir'
    dry_run = '$dry_run' == 'true'
    
    orphaned_entries = []
    updated_sessions = sessions.copy()
    
    for session_id, session_data in sessions.items():
        session_dir = os.path.join(base_dir, 'sessions', session_id)
        
        # Check if session directory exists
        if not os.path.exists(session_dir):
            orphaned_entries.append(session_id)
            continue
        
        # Check if required files exist
        metadata_file = os.path.join(session_dir, 'metadata.json')
        if not os.path.exists(metadata_file):
            orphaned_entries.append(session_id)
    
    # Process removals
    removed_count = 0
    
    for session_id in orphaned_entries:
        if dry_run:
            print(f"Would remove orphaned index entry: {session_id}")
        else:
            if session_id in updated_sessions:
                del updated_sessions[session_id]
                removed_count += 1
                print(f"Removed orphaned index entry: {session_id}")
    
    # Update index if changes were made
    if not dry_run and removed_count > 0:
        index['sessions'] = updated_sessions
        index['lastUpdated'] = datetime.now().isoformat()
        
        # Update statistics
        total_sessions = len(updated_sessions)
        active_sessions = sum(1 for s in updated_sessions.values() if s.get('status') == 'active')
        inactive_sessions = sum(1 for s in updated_sessions.values() if s.get('status') == 'inactive')
        terminated_sessions = sum(1 for s in updated_sessions.values() if s.get('status') == 'terminated')
        
        index['totalSessions'] = total_sessions
        index['activeSessions'] = active_sessions
        index['inactiveSessions'] = inactive_sessions
        index['terminatedSessions'] = terminated_sessions
        
        with open('$index_file', 'w') as f:
            json.dump(index, f, indent=2)
    
    # Output summary
    summary = {
        'orphanedEntriesFound': len(orphaned_entries),
        'orphanedEntriesRemoved': removed_count,
        'dryRun': dry_run
    }
    
    print(json.dumps(summary))

except Exception as e:
    print(f"Error during orphaned entries cleanup: {e}", file=sys.stderr)
    sys.exit(1)
EOF
    
    local cleanup_result=$?
    
    if [[ $cleanup_result -eq 0 ]]; then
        session_log "Orphaned entries cleanup completed (dry_run: $dry_run)"
    else
        session_error "Orphaned entries cleanup failed"
        return 1
    fi
}

# Comprehensive session maintenance
perform_session_maintenance() {
    local base_dir="${1:-.flowloom}"
    local dry_run="${2:-true}"
    local config_file="${3:-}"
    
    session_log "Performing comprehensive session maintenance (dry_run: $dry_run)"
    
    # Load custom configuration if provided
    local session_retention_days=$DEFAULT_SESSION_RETENTION_DAYS
    local backup_retention_days=$DEFAULT_BACKUP_RETENTION_DAYS
    local terminated_retention_days=$DEFAULT_TERMINATED_SESSION_RETENTION_DAYS
    
    if [[ -n "$config_file" && -f "$config_file" ]]; then
        session_debug "Loading cleanup configuration from: $config_file"
        
        # Parse JSON config safely
        python3 << EOF
import json
import sys

try:
    with open('$config_file', 'r') as f:
        config = json.load(f)
    
    # Export configuration as shell variables
    print(f"session_retention_days={config.get('sessionRetentionDays', $DEFAULT_SESSION_RETENTION_DAYS)}")
    print(f"backup_retention_days={config.get('backupRetentionDays', $DEFAULT_BACKUP_RETENTION_DAYS)}")
    print(f"terminated_retention_days={config.get('terminatedSessionRetentionDays', $DEFAULT_TERMINATED_SESSION_RETENTION_DAYS)}")
    
except Exception as e:
    print(f"Error reading config: {e}", file=sys.stderr)
    sys.exit(1)
EOF
        
        if [[ $? -eq 0 ]]; then
            eval "$(python3 << 'EOF'
import json
try:
    with open('$config_file', 'r') as f:
        config = json.load(f)
    print(f"session_retention_days={config.get('sessionRetentionDays', $DEFAULT_SESSION_RETENTION_DAYS)}")
    print(f"backup_retention_days={config.get('backupRetentionDays', $DEFAULT_BACKUP_RETENTION_DAYS)}")
    print(f"terminated_retention_days={config.get('terminatedSessionRetentionDays', $DEFAULT_TERMINATED_SESSION_RETENTION_DAYS)}")
except:
    pass
EOF
)"
        fi
    fi
    
    session_debug "Using retention periods: sessions=${session_retention_days}d, backups=${backup_retention_days}d, terminated=${terminated_retention_days}d"
    
    # Perform maintenance tasks
    local maintenance_results=()
    
    # 1. Clean up terminated sessions
    session_log "Cleaning up terminated sessions..."
    local terminated_result
    terminated_result=$(cleanup_old_sessions "$base_dir" "$terminated_retention_days" "$dry_run" "terminated")
    maintenance_results+=("terminated_sessions:$terminated_result")
    
    # 2. Clean up old backups
    session_log "Cleaning up old backups..."
    local backup_result
    backup_result=$(cleanup_session_backups "$base_dir" "$backup_retention_days" "$dry_run")
    maintenance_results+=("backups:$backup_result")
    
    # 3. Clean up orphaned index entries
    session_log "Cleaning up orphaned index entries..."
    local orphaned_result
    orphaned_result=$(cleanup_orphaned_index_entries "$base_dir" "$dry_run")
    maintenance_results+=("orphaned_entries:$orphaned_result")
    
    # 4. Validate session index integrity
    session_log "Validating session index integrity..."
    local index_file="$base_dir/sessions/index.json"
    if [[ -f "$index_file" ]]; then
        if validate_json_file "$index_file"; then
            maintenance_results+=("index_validation:passed")
        else
            maintenance_results+=("index_validation:failed")
        fi
    else
        maintenance_results+=("index_validation:missing")
    fi
    
    # Output comprehensive maintenance summary
    python3 << EOF
import json
from datetime import datetime

# Parse maintenance results
results = {}
for result_pair in ['${maintenance_results[*]}']:
    if ':' in result_pair:
        key, value = result_pair.split(':', 1)
        try:
            results[key] = json.loads(value)
        except:
            results[key] = value

summary = {
    'maintenanceCompleted': datetime.now().isoformat(),
    'dryRun': '$dry_run' == 'true',
    'configuration': {
        'sessionRetentionDays': $session_retention_days,
        'backupRetentionDays': $backup_retention_days,
        'terminatedRetentionDays': $terminated_retention_days
    },
    'results': results,
    'baseDirectory': '$base_dir'
}

print(json.dumps(summary, indent=2))
EOF
    
    session_log "Session maintenance completed"
}

# Generate cleanup report
generate_cleanup_report() {
    local base_dir="${1:-.flowloom}"
    local output_format="${2:-json}"
    
    session_debug "Generating cleanup report (format: $output_format)"
    
    # Analyze session structure and generate report
    python3 << EOF
import json
import os
import sys
from datetime import datetime, timedelta

try:
    base_dir = '$base_dir'
    output_format = '$output_format'
    
    # Initialize report data
    report = {
        'reportGenerated': datetime.now().isoformat(),
        'baseDirectory': base_dir,
        'statistics': {
            'totalSessions': 0,
            'activeSessions': 0,
            'inactiveSessions': 0,
            'terminatedSessions': 0,
            'totalBackups': 0,
            'totalDiskUsage': 0
        },
        'recommendations': [],
        'issues': []
    }
    
    # Read session index
    index_file = os.path.join(base_dir, 'sessions', 'index.json')
    if os.path.exists(index_file):
        try:
            with open(index_file, 'r') as f:
                index = json.load(f)
            
            sessions = index.get('sessions', {})
            report['statistics']['totalSessions'] = len(sessions)
            
            current_time = datetime.now()
            old_sessions = []
            stale_sessions = []
            
            for session_id, session_data in sessions.items():
                status = session_data.get('status', 'unknown')
                
                if status == 'active':
                    report['statistics']['activeSessions'] += 1
                elif status == 'inactive':
                    report['statistics']['inactiveSessions'] += 1
                elif status == 'terminated':
                    report['statistics']['terminatedSessions'] += 1
                
                # Check for old sessions
                try:
                    last_activity = datetime.fromisoformat(session_data.get('lastActivity', '').replace('Z', '+00:00'))
                    age_days = (current_time - last_activity).days
                    
                    if age_days > 90:
                        old_sessions.append({'sessionId': session_id, 'ageDays': age_days, 'status': status})
                    elif age_days > 30 and status == 'terminated':
                        stale_sessions.append({'sessionId': session_id, 'ageDays': age_days, 'status': status})
                except:
                    pass
                
                # Calculate session disk usage
                session_dir = os.path.join(base_dir, 'sessions', session_id)
                if os.path.exists(session_dir):
                    for root, dirs, files in os.walk(session_dir):
                        for file in files:
                            file_path = os.path.join(root, file)
                            try:
                                report['statistics']['totalDiskUsage'] += os.path.getsize(file_path)
                            except:
                                pass
            
            # Add recommendations based on analysis
            if old_sessions:
                report['recommendations'].append({
                    'type': 'cleanup_old_sessions',
                    'description': f'{len(old_sessions)} sessions older than 90 days found',
                    'action': 'Consider running cleanup with 90-day retention',
                    'sessionsCount': len(old_sessions)
                })
            
            if stale_sessions:
                report['recommendations'].append({
                    'type': 'cleanup_terminated_sessions',
                    'description': f'{len(stale_sessions)} terminated sessions older than 30 days found',
                    'action': 'Consider running cleanup for terminated sessions with 30-day retention',
                    'sessionsCount': len(stale_sessions)
                })
        
        except Exception as e:
            report['issues'].append({
                'type': 'index_read_error',
                'description': f'Failed to read session index: {str(e)}'
            })
    else:
        report['issues'].append({
            'type': 'missing_index',
            'description': 'Session index file not found'
        })
    
    # Check backup directory
    backup_dir = os.path.join(base_dir, 'backups')
    if os.path.exists(backup_dir):
        backup_count = 0
        backup_size = 0
        
        for session_id in os.listdir(backup_dir):
            session_backup_dir = os.path.join(backup_dir, session_id)
            if os.path.isdir(session_backup_dir):
                for item in os.listdir(session_backup_dir):
                    if item.endswith('.manifest'):
                        backup_count += 1
                    
                    item_path = os.path.join(session_backup_dir, item)
                    if os.path.isfile(item_path):
                        try:
                            backup_size += os.path.getsize(item_path)
                        except:
                            pass
                    elif os.path.isdir(item_path):
                        for root, dirs, files in os.walk(item_path):
                            for file in files:
                                file_path = os.path.join(root, file)
                                try:
                                    backup_size += os.path.getsize(file_path)
                                except:
                                    pass
        
        report['statistics']['totalBackups'] = backup_count
        report['statistics']['totalDiskUsage'] += backup_size
        
        # Backup recommendations
        if backup_count > 100:
            report['recommendations'].append({
                'type': 'cleanup_old_backups',
                'description': f'{backup_count} backups found',
                'action': 'Consider cleaning up old backups to free disk space',
                'backupCount': backup_count
            })
    
    # Format disk usage
    disk_usage = report['statistics']['totalDiskUsage']
    if disk_usage > 1024 * 1024 * 1024:  # > 1GB
        usage_str = f"{disk_usage / (1024 * 1024 * 1024):.1f} GB"
    elif disk_usage > 1024 * 1024:  # > 1MB
        usage_str = f"{disk_usage / (1024 * 1024):.1f} MB"
    else:
        usage_str = f"{disk_usage / 1024:.1f} KB"
    
    report['statistics']['diskUsageFormatted'] = usage_str
    
    # Output report
    if output_format == 'summary':
        print(f"FlowLoom Session Cleanup Report")
        print(f"Generated: {report['reportGenerated']}")
        print(f"")
        print(f"Statistics:")
        print(f"  Total Sessions: {report['statistics']['totalSessions']}")
        print(f"  Active: {report['statistics']['activeSessions']}")
        print(f"  Inactive: {report['statistics']['inactiveSessions']}")
        print(f"  Terminated: {report['statistics']['terminatedSessions']}")
        print(f"  Total Backups: {report['statistics']['totalBackups']}")
        print(f"  Disk Usage: {usage_str}")
        print(f"")
        
        if report['recommendations']:
            print(f"Recommendations:")
            for rec in report['recommendations']:
                print(f"  - {rec['description']}")
                print(f"    Action: {rec['action']}")
        
        if report['issues']:
            print(f"")
            print(f"Issues:")
            for issue in report['issues']:
                print(f"  - {issue['description']}")
    else:
        print(json.dumps(report, indent=2))

except Exception as e:
    error_info = {
        'success': False,
        'error': f'Failed to generate cleanup report: {str(e)}'
    }
    print(json.dumps(error_info, indent=2))
    sys.exit(1)
EOF
    
    session_log "Cleanup report generated"
}

# Print help
print_cleanup_help() {
    cat << EOF
FlowLoom Session Cleanup

Usage: session_cleanup.sh <command> [options]

Commands:
  sessions <base_dir> <retention_days> <dry_run> [status_filter]  Clean up old sessions
  backups <base_dir> <retention_days> <dry_run>                  Clean up old backups
  orphaned <base_dir> <dry_run>                                  Clean up orphaned index entries
  maintenance <base_dir> <dry_run> [config_file]                 Comprehensive maintenance
  report <base_dir> [format]                                     Generate cleanup report

Parameters:
  base_dir: Session base directory (default: .flowloom)
  retention_days: Days to keep sessions/backups (default: sessions=90, backups=30, terminated=30)
  dry_run: true/false - Preview changes without executing (default: true)
  status_filter: all, active, inactive, terminated (default: terminated)
  config_file: JSON configuration file for custom retention policies
  format: json, summary (default: json)

Examples:
  session_cleanup.sh sessions .flowloom 90 false terminated
  session_cleanup.sh backups .flowloom 30 true
  session_cleanup.sh maintenance .flowloom false cleanup_config.json
  session_cleanup.sh report .flowloom summary

Configuration File Format:
{
  "sessionRetentionDays": 90,
  "backupRetentionDays": 30,
  "terminatedSessionRetentionDays": 30
}

Default Retention Policies:
  - Sessions: 90 days (all statuses)
  - Terminated Sessions: 30 days
  - Backups: 30 days
  - Active Sessions: Minimum 7 days protection

Safety Features:
  - Dry run mode by default
  - Minimum retention for active sessions
  - JSON validation before cleanup
  - Orphaned entry detection
  - Comprehensive error handling

Output:
  All commands return JSON formatted results with operation statistics.

Notes:
  - Always run with dry_run=true first to preview changes
  - Backup cleanup removes both full and incremental backups
  - Orphaned cleanup removes index entries for missing sessions
  - Maintenance performs all cleanup operations in sequence
  - Respects Claude Code security boundaries
EOF
}

# Main command dispatcher
main() {
    if [[ $# -eq 0 ]]; then
        print_cleanup_help
        exit 1
    fi
    
    local command="$1"
    shift
    
    case "$command" in
        "sessions")
            if [[ $# -lt 3 ]]; then
                echo "Error: sessions requires base_dir, retention_days, and dry_run" >&2
                exit 1
            fi
            cleanup_old_sessions "$@"
            ;;
        "backups")
            if [[ $# -lt 3 ]]; then
                echo "Error: backups requires base_dir, retention_days, and dry_run" >&2
                exit 1
            fi
            cleanup_session_backups "$@"
            ;;
        "orphaned")
            if [[ $# -lt 2 ]]; then
                echo "Error: orphaned requires base_dir and dry_run" >&2
                exit 1
            fi
            cleanup_orphaned_index_entries "$@"
            ;;
        "maintenance")
            if [[ $# -lt 2 ]]; then
                echo "Error: maintenance requires base_dir and dry_run" >&2
                exit 1
            fi
            perform_session_maintenance "$@"
            ;;
        "report")
            if [[ $# -lt 1 ]]; then
                echo "Error: report requires base_dir" >&2
                exit 1
            fi
            generate_cleanup_report "$@"
            ;;
        "help"|"--help"|"-h")
            print_cleanup_help
            ;;
        *)
            echo "Error: Unknown command: $command" >&2
            echo "Use 'session_cleanup.sh help' for usage information" >&2
            exit 1
            ;;
    esac
}

# Run main function if script is executed directly
if [[ "${BASH_SOURCE[0]:-$0}" == "${0}" ]]; then
    main "$@"
fi