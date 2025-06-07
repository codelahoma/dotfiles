#!/bin/bash
# bin/session_validation.sh - JSON schema validation with Python
# Part of FlowLoom Session Management System - Phase 2: State Management
# Based on Plan 3120 - Session Tracking Detailed Design
#
# SECURITY & ETHICS NOTICE:
# This script provides validation for session data while respecting Claude Code's
# security boundaries. All operations are confined to designated session directories
# and do not access unauthorized resources or attempt to circumvent security measures.

set -euo pipefail

# Import session utilities
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
source "$SCRIPT_DIR/session_utils.sh"

# Validation functions

# Validate session metadata JSON structure
validate_session_metadata() {
    local metadata_file="$1"
    local strict_mode="${2:-false}"
    
    session_debug "Validating session metadata: $metadata_file (strict: $strict_mode)"
    
    if [[ ! -f "$metadata_file" ]]; then
        session_error "Metadata file not found: $metadata_file"
        return 1
    fi
    
    python3 << EOF
import json
import sys
import re
from datetime import datetime

def validate_metadata(metadata_file, strict_mode=False):
    try:
        with open(metadata_file, 'r') as f:
            data = json.load(f)
    except (json.JSONDecodeError, FileNotFoundError) as e:
        print(f"Error: Invalid JSON or file not found: {e}", file=sys.stderr)
        return False
    
    errors = []
    warnings = []
    
    # Required fields
    required_fields = [
        'sessionId', 'ppid', 'startTime', 'status', 
        'workingDirectory', 'gitBranch', 'activeMode', 
        'sessionContext', 'lastActivity'
    ]
    
    # Check required fields
    for field in required_fields:
        if field not in data:
            errors.append(f"Missing required field: {field}")
    
    if not errors:  # Only validate content if structure is correct
        # Validate session ID format
        # Support both new format (PPID-purpose) and legacy format (PPID-6char)
        session_id_pattern = r'^(session_)?\d+-([a-z0-9]([a-z0-9-]*[a-z0-9])?|[a-z0-9]{6})$'
        if 'sessionId' in data and not re.match(session_id_pattern, data['sessionId']):
            errors.append(f"Invalid session ID format: {data['sessionId']}")
        
        # Validate status enum
        valid_statuses = ['active', 'inactive', 'terminated']
        if 'status' in data and data['status'] not in valid_statuses:
            errors.append(f"Invalid status: {data['status']} (must be one of: {', '.join(valid_statuses)})")
        
        # Validate PPID is a positive integer
        if 'ppid' in data:
            if not isinstance(data['ppid'], int) or data['ppid'] < 1:
                errors.append(f"Invalid PPID: {data['ppid']} (must be positive integer)")
        
        # Validate timestamp formats
        timestamp_fields = ['startTime', 'lastActivity', 'endTime']
        for field in timestamp_fields:
            if field in data and data[field] is not None:
                try:
                    datetime.fromisoformat(data[field].replace('Z', '+00:00'))
                except (ValueError, AttributeError):
                    errors.append(f"Invalid timestamp format for {field}: {data[field]}")
        
        # Validate working directory (should be absolute path)
        if 'workingDirectory' in data:
            if not data['workingDirectory'].startswith('/'):
                if strict_mode:
                    errors.append(f"Working directory must be absolute path: {data['workingDirectory']}")
                else:
                    warnings.append(f"Working directory should be absolute path: {data['workingDirectory']}")
        
        # Validate git branch name
        if 'gitBranch' in data:
            invalid_chars = ['..', ' ', '~', '^', ':', '?', '*', '[', '\\\\']
            branch_name = data['gitBranch']
            for char in invalid_chars:
                if char in branch_name:
                    errors.append(f"Invalid characters in git branch name: {branch_name}")
                    break
        
        # Optional field validations
        if 'commandCount' in data:
            if not isinstance(data['commandCount'], int) or data['commandCount'] < 0:
                errors.append(f"Invalid command count: {data['commandCount']} (must be non-negative integer)")
        
        if 'memoryEntities' in data:
            if not isinstance(data['memoryEntities'], list):
                errors.append(f"memoryEntities must be an array")
        
        if 'todoItems' in data:
            if not isinstance(data['todoItems'], list):
                errors.append(f"todoItems must be an array")
        
        if 'artifacts' in data:
            if not isinstance(data['artifacts'], list):
                errors.append(f"artifacts must be an array")
    
    # Report results
    if errors:
        print("VALIDATION FAILED", file=sys.stderr)
        for error in errors:
            print(f"ERROR: {error}", file=sys.stderr)
    
    if warnings:
        for warning in warnings:
            print(f"WARNING: {warning}", file=sys.stderr)
    
    if not errors:
        print("VALIDATION PASSED")
        if warnings:
            print(f"({len(warnings)} warnings)")
        return True
    else:
        print(f"({len(errors)} errors, {len(warnings)} warnings)", file=sys.stderr)
        return False

if __name__ == "__main__":
    import sys
    strict = "$strict_mode" == "true"
    if validate_metadata("$metadata_file", strict):
        sys.exit(0)
    else:
        sys.exit(1)
EOF
}

# Validate session state JSON structure
validate_session_state() {
    local state_file="$1"
    local strict_mode="${2:-false}"
    
    session_debug "Validating session state: $state_file (strict: $strict_mode)"
    
    if [[ ! -f "$state_file" ]]; then
        session_error "State file not found: $state_file"
        return 1
    fi
    
    python3 << EOF
import json
import sys
from datetime import datetime

def validate_state(state_file, strict_mode=False):
    try:
        with open(state_file, 'r') as f:
            state = json.load(f)
    except (json.JSONDecodeError, FileNotFoundError) as e:
        print(f"Error: Invalid JSON or file not found: {e}", file=sys.stderr)
        return False
    
    errors = []
    warnings = []
    
    # Check required top-level fields
    required_top_fields = ['version', 'timestamp', 'sessionId', 'data']
    for field in required_top_fields:
        if field not in state:
            errors.append(f"Missing required top-level field: {field}")
    
    if 'data' not in state:
        errors.append("No 'data' field found - cannot validate state structure")
        print("VALIDATION FAILED", file=sys.stderr)
        for error in errors:
            print(f"ERROR: {error}", file=sys.stderr)
        return False
    
    data = state['data']
    
    # Check required data fields
    required_data_fields = [
        'currentPlan', 'activeCommands', 'recentFiles', 
        'gitState', 'modeHistory', 'contextHistory'
    ]
    
    for field in required_data_fields:
        if field not in data:
            errors.append(f"Missing required data field: {field}")
    
    # Validate data field types
    if 'activeCommands' in data and not isinstance(data['activeCommands'], list):
        errors.append("activeCommands must be an array")
    
    if 'recentFiles' in data and not isinstance(data['recentFiles'], list):
        errors.append("recentFiles must be an array")
    
    if 'modeHistory' in data and not isinstance(data['modeHistory'], list):
        errors.append("modeHistory must be an array")
    
    if 'contextHistory' in data and not isinstance(data['contextHistory'], list):
        errors.append("contextHistory must be an array")
    
    # Validate gitState structure
    if 'gitState' in data:
        git_state = data['gitState']
        if not isinstance(git_state, dict):
            errors.append("gitState must be an object")
        else:
            required_git_fields = ['branch', 'uncommittedFiles', 'lastCommit']
            for field in required_git_fields:
                if field not in git_state:
                    errors.append(f"Missing gitState field: {field}")
            
            if 'uncommittedFiles' in git_state and not isinstance(git_state['uncommittedFiles'], list):
                errors.append("gitState.uncommittedFiles must be an array")
    
    # Validate optional fields
    if 'artifacts' in data and not isinstance(data['artifacts'], list):
        errors.append("artifacts must be an array")
    
    if 'todoItems' in data and not isinstance(data['todoItems'], list):
        errors.append("todoItems must be an array")
    
    if 'memoryEntities' in data and not isinstance(data['memoryEntities'], list):
        errors.append("memoryEntities must be an array")
    
    # Validate version
    if 'version' in state:
        if not isinstance(state['version'], int) or state['version'] < 1:
            errors.append(f"Invalid version: {state['version']} (must be positive integer)")
    
    # Validate timestamp
    if 'timestamp' in state:
        try:
            datetime.fromisoformat(state['timestamp'].replace('Z', '+00:00'))
        except (ValueError, AttributeError):
            errors.append(f"Invalid timestamp format: {state['timestamp']}")
    
    # Validate session ID format
    if 'sessionId' in state:
        import re
        # Support both new format (PPID-purpose) and legacy format (PPID-6char)
        session_id_pattern = r'^(session_)?\d+-([a-z0-9]([a-z0-9-]*[a-z0-9])?|[a-z0-9]{6})$'
        if not re.match(session_id_pattern, state['sessionId']):
            errors.append(f"Invalid session ID format: {state['sessionId']}")
    
    # Additional strict mode validations
    if strict_mode:
        # Validate recent files are absolute paths
        if 'recentFiles' in data:
            for i, file_path in enumerate(data['recentFiles']):
                if isinstance(file_path, str) and not file_path.startswith('/'):
                    warnings.append(f"recentFiles[{i}] should be absolute path: {file_path}")
        
        # Validate git branch name
        if 'gitState' in data and 'branch' in data['gitState']:
            branch_name = data['gitState']['branch']
            if isinstance(branch_name, str):
                invalid_chars = ['..', ' ', '~', '^', ':', '?', '*', '[', '\\\\']
                for char in invalid_chars:
                    if char in branch_name:
                        errors.append(f"Invalid characters in git branch name: {branch_name}")
                        break
    
    # Report results
    if errors:
        print("VALIDATION FAILED", file=sys.stderr)
        for error in errors:
            print(f"ERROR: {error}", file=sys.stderr)
    
    if warnings:
        for warning in warnings:
            print(f"WARNING: {warning}", file=sys.stderr)
    
    if not errors:
        print("VALIDATION PASSED")
        if warnings:
            print(f"({len(warnings)} warnings)")
        return True
    else:
        print(f"({len(errors)} errors, {len(warnings)} warnings)", file=sys.stderr)
        return False

if __name__ == "__main__":
    strict = "$strict_mode" == "true"
    if validate_state("$state_file", strict):
        sys.exit(0)
    else:
        sys.exit(1)
EOF
}

# Validate session index JSON structure
validate_session_index() {
    local index_file="$1"
    local strict_mode="${2:-false}"
    
    session_debug "Validating session index: $index_file (strict: $strict_mode)"
    
    if [[ ! -f "$index_file" ]]; then
        session_error "Index file not found: $index_file"
        return 1
    fi
    
    python3 << EOF
import json
import sys
import re
from datetime import datetime

def validate_index(index_file, strict_mode=False):
    try:
        with open(index_file, 'r') as f:
            index = json.load(f)
    except (json.JSONDecodeError, FileNotFoundError) as e:
        print(f"Error: Invalid JSON or file not found: {e}", file=sys.stderr)
        return False
    
    errors = []
    warnings = []
    
    # Check required top-level fields
    required_fields = ['sessions', 'lastUpdated']
    for field in required_fields:
        if field not in index:
            errors.append(f"Missing required field: {field}")
    
    # Validate sessions structure
    if 'sessions' in index:
        sessions = index['sessions']
        if not isinstance(sessions, dict):
            errors.append("sessions must be an object")
        else:
            # Support both new format (PPID-purpose) and legacy format (PPID-6char)
        session_id_pattern = r'^(session_)?\d+-([a-z0-9]([a-z0-9-]*[a-z0-9])?|[a-z0-9]{6})$'
            
            for session_id, session_data in sessions.items():
                # Validate session ID format
                if not re.match(session_id_pattern, session_id):
                    errors.append(f"Invalid session ID format: {session_id}")
                
                # Validate session entry structure
                if not isinstance(session_data, dict):
                    errors.append(f"Session entry must be object: {session_id}")
                    continue
                
                required_session_fields = [
                    'sessionId', 'ppid', 'status', 'startTime', 
                    'lastActivity', 'workingDirectory', 'gitBranch'
                ]
                
                for field in required_session_fields:
                    if field not in session_data:
                        errors.append(f"Missing field in session {session_id}: {field}")
                
                # Validate specific field types and values
                if 'ppid' in session_data:
                    if not isinstance(session_data['ppid'], int) or session_data['ppid'] < 1:
                        errors.append(f"Invalid PPID in session {session_id}: {session_data['ppid']}")
                
                if 'status' in session_data:
                    valid_statuses = ['active', 'inactive', 'terminated']
                    if session_data['status'] not in valid_statuses:
                        errors.append(f"Invalid status in session {session_id}: {session_data['status']}")
                
                # Validate timestamps
                timestamp_fields = ['startTime', 'lastActivity']
                for field in timestamp_fields:
                    if field in session_data and session_data[field] is not None:
                        try:
                            datetime.fromisoformat(session_data[field].replace('Z', '+00:00'))
                        except (ValueError, AttributeError):
                            errors.append(f"Invalid {field} in session {session_id}: {session_data[field]}")
    
    # Validate optional statistics fields
    numeric_fields = ['totalSessions', 'activeSessions', 'inactiveSessions', 'terminatedSessions']
    for field in numeric_fields:
        if field in index:
            if not isinstance(index[field], int) or index[field] < 0:
                errors.append(f"Invalid {field}: {index[field]} (must be non-negative integer)")
    
    # Validate lastUpdated timestamp
    if 'lastUpdated' in index:
        try:
            datetime.fromisoformat(index['lastUpdated'].replace('Z', '+00:00'))
        except (ValueError, AttributeError):
            errors.append(f"Invalid lastUpdated timestamp: {index['lastUpdated']}")
    
    # Cross-validation checks
    if 'sessions' in index and isinstance(index['sessions'], dict):
        actual_total = len(index['sessions'])
        actual_active = sum(1 for s in index['sessions'].values() if s.get('status') == 'active')
        actual_inactive = sum(1 for s in index['sessions'].values() if s.get('status') == 'inactive')
        actual_terminated = sum(1 for s in index['sessions'].values() if s.get('status') == 'terminated')
        
        if 'totalSessions' in index and index['totalSessions'] != actual_total:
            if strict_mode:
                errors.append(f"totalSessions mismatch: index says {index['totalSessions']}, actual: {actual_total}")
            else:
                warnings.append(f"totalSessions mismatch: index says {index['totalSessions']}, actual: {actual_total}")
        
        if 'activeSessions' in index and index['activeSessions'] != actual_active:
            if strict_mode:
                errors.append(f"activeSessions mismatch: index says {index['activeSessions']}, actual: {actual_active}")
            else:
                warnings.append(f"activeSessions mismatch: index says {index['activeSessions']}, actual: {actual_active}")
    
    # Report results
    if errors:
        print("VALIDATION FAILED", file=sys.stderr)
        for error in errors:
            print(f"ERROR: {error}", file=sys.stderr)
    
    if warnings:
        for warning in warnings:
            print(f"WARNING: {warning}", file=sys.stderr)
    
    if not errors:
        print("VALIDATION PASSED")
        if warnings:
            print(f"({len(warnings)} warnings)")
        return True
    else:
        print(f"({len(errors)} errors, {len(warnings)} warnings)", file=sys.stderr)
        return False

if __name__ == "__main__":
    strict = "$strict_mode" == "true"
    if validate_index("$index_file", strict):
        sys.exit(0)
    else:
        sys.exit(1)
EOF
}

# Validate session backup manifest
validate_backup_manifest() {
    local manifest_file="$1"
    local strict_mode="${2:-false}"
    
    session_debug "Validating backup manifest: $manifest_file (strict: $strict_mode)"
    
    if [[ ! -f "$manifest_file" ]]; then
        session_error "Manifest file not found: $manifest_file"
        return 1
    fi
    
    python3 << EOF
import json
import sys
from datetime import datetime

def validate_manifest(manifest_file, strict_mode=False):
    try:
        with open(manifest_file, 'r') as f:
            manifest = json.load(f)
    except (json.JSONDecodeError, FileNotFoundError) as e:
        print(f"Error: Invalid JSON or file not found: {e}", file=sys.stderr)
        return False
    
    errors = []
    warnings = []
    
    # Check required fields
    required_fields = ['backupId', 'sessionId', 'timestamp', 'type']
    for field in required_fields:
        if field not in manifest:
            errors.append(f"Missing required field: {field}")
    
    # Validate backup type
    if 'type' in manifest:
        valid_types = ['full', 'incremental']
        if manifest['type'] not in valid_types:
            errors.append(f"Invalid backup type: {manifest['type']} (must be one of: {', '.join(valid_types)})")
    
    # Validate session ID format
    if 'sessionId' in manifest:
        import re
        # Support both new format (PPID-purpose) and legacy format (PPID-6char)
        session_id_pattern = r'^(session_)?\d+-([a-z0-9]([a-z0-9-]*[a-z0-9])?|[a-z0-9]{6})$'
        if not re.match(session_id_pattern, manifest['sessionId']):
            errors.append(f"Invalid session ID format: {manifest['sessionId']}")
    
    # Validate timestamp
    if 'timestamp' in manifest:
        try:
            datetime.fromisoformat(manifest['timestamp'].replace('Z', '+00:00'))
        except (ValueError, AttributeError):
            errors.append(f"Invalid timestamp format: {manifest['timestamp']}")
    
    # Type-specific validations
    if 'type' in manifest:
        if manifest['type'] == 'full':
            # Full backup should have archiveFile
            if 'archiveFile' not in manifest:
                errors.append("Full backup manifest missing archiveFile")
            
            if 'archiveSize' in manifest:
                if not isinstance(manifest['archiveSize'], int) or manifest['archiveSize'] < 0:
                    errors.append(f"Invalid archiveSize: {manifest['archiveSize']}")
        
        elif manifest['type'] == 'incremental':
            # Incremental backup should have fileCount
            if 'fileCount' in manifest:
                if not isinstance(manifest['fileCount'], int) or manifest['fileCount'] < 0:
                    errors.append(f"Invalid fileCount: {manifest['fileCount']}")
    
    # Validate optional fields
    if 'files' in manifest:
        if not isinstance(manifest['files'], list):
            errors.append("files must be an array")
    
    if 'status' in manifest:
        valid_statuses = ['completed', 'failed', 'in_progress']
        if manifest['status'] not in valid_statuses:
            errors.append(f"Invalid status: {manifest['status']} (must be one of: {', '.join(valid_statuses)})")
    
    # Report results
    if errors:
        print("VALIDATION FAILED", file=sys.stderr)
        for error in errors:
            print(f"ERROR: {error}", file=sys.stderr)
    
    if warnings:
        for warning in warnings:
            print(f"WARNING: {warning}", file=sys.stderr)
    
    if not errors:
        print("VALIDATION PASSED")
        if warnings:
            print(f"({len(warnings)} warnings)")
        return True
    else:
        print(f"({len(errors)} errors, {len(warnings)} warnings)", file=sys.stderr)
        return False

if __name__ == "__main__":
    strict = "$strict_mode" == "true"
    if validate_manifest("$manifest_file", strict):
        sys.exit(0)
    else:
        sys.exit(1)
EOF
}

# Validate complete session structure
validate_complete_session() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    local strict_mode="${3:-false}"
    
    session_debug "Validating complete session: $session_id (strict: $strict_mode)"
    
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
    
    local validation_errors=0
    
    echo "=== Validating Complete Session: $session_id ==="
    
    # Validate directory structure
    echo "Checking directory structure..."
    if ! validate_session_structure "$session_id" "$base_dir"; then
        echo "ERROR: Invalid session directory structure"
        ((validation_errors++))
    else
        echo "✓ Directory structure valid"
    fi
    
    # Validate metadata
    echo "Validating metadata..."
    local metadata_file="$session_dir/metadata.json"
    if [[ -f "$metadata_file" ]]; then
        if validate_session_metadata "$metadata_file" "$strict_mode"; then
            echo "✓ Metadata valid"
        else
            echo "ERROR: Invalid metadata"
            ((validation_errors++))
        fi
    else
        echo "ERROR: Metadata file missing"
        ((validation_errors++))
    fi
    
    # Validate state
    echo "Validating state..."
    local state_file="$session_dir/state.json"
    if [[ -f "$state_file" ]]; then
        if validate_session_state "$state_file" "$strict_mode"; then
            echo "✓ State valid"
        else
            echo "ERROR: Invalid state"
            ((validation_errors++))
        fi
    else
        echo "WARNING: State file missing (will be created on first use)"
    fi
    
    # Validate index entry
    echo "Validating index entry..."
    local index_file="$base_dir/sessions/index.json"
    if [[ -f "$index_file" ]]; then
        if validate_session_index "$index_file" "$strict_mode"; then
            echo "✓ Index valid"
            
            # Check if session is in index
            python3 << EOF
import json
with open('$index_file', 'r') as f:
    index = json.load(f)

if '$session_id' in index.get('sessions', {}):
    print("✓ Session found in index")
else:
    print("WARNING: Session not found in index")
EOF
        else
            echo "ERROR: Invalid index"
            ((validation_errors++))
        fi
    else
        echo "ERROR: Index file missing"
        ((validation_errors++))
    fi
    
    # Validate backups if any exist
    local backup_dir="$base_dir/backups/$session_id"
    if [[ -d "$backup_dir" ]]; then
        echo "Validating backups..."
        local backup_errors=0
        
        while IFS= read -r -d '' manifest_file; do
            if ! validate_backup_manifest "$manifest_file" "$strict_mode"; then
                ((backup_errors++))
            fi
        done < <(find "$backup_dir" -name "*.manifest" -type f -print0 2>/dev/null)
        
        if [[ $backup_errors -eq 0 ]]; then
            local backup_count
            backup_count=$(find "$backup_dir" -name "*.manifest" -type f | wc -l)
            echo "✓ All $backup_count backup manifests valid"
        else
            echo "ERROR: $backup_errors invalid backup manifests"
            ((validation_errors++))
        fi
    fi
    
    echo "=== Validation Complete ==="
    
    if [[ $validation_errors -eq 0 ]]; then
        echo "✓ Session validation PASSED"
        return 0
    else
        echo "✗ Session validation FAILED ($validation_errors errors)"
        return 1
    fi
}

# Repair session structure issues
repair_session() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    local dry_run="${3:-true}"
    
    session_debug "Repairing session: $session_id (dry_run: $dry_run)"
    
    local session_dir
    session_dir=$(get_session_directory "$session_id" "$base_dir")
    
    local repairs_needed=0
    local repairs_made=0
    
    echo "=== Session Repair Analysis: $session_id ==="
    
    # Check and repair directory structure
    if [[ ! -d "$session_dir" ]]; then
        echo "REPAIR NEEDED: Session directory missing"
        ((repairs_needed++))
        
        if [[ "$dry_run" == "false" ]]; then
            mkdir -p "$session_dir/artifacts"
            echo "REPAIRED: Created session directory"
            ((repairs_made++))
        fi
    fi
    
    if [[ ! -d "$session_dir/artifacts" ]]; then
        echo "REPAIR NEEDED: Artifacts directory missing"
        ((repairs_needed++))
        
        if [[ "$dry_run" == "false" ]]; then
            mkdir -p "$session_dir/artifacts"
            echo "REPAIRED: Created artifacts directory"
            ((repairs_made++))
        fi
    fi
    
    # Check and repair metadata
    local metadata_file="$session_dir/metadata.json"
    if [[ ! -f "$metadata_file" ]]; then
        echo "REPAIR NEEDED: Metadata file missing"
        ((repairs_needed++))
        
        if [[ "$dry_run" == "false" ]]; then
            # Create default metadata
            python3 << EOF
import json
from datetime import datetime

metadata = {
    "sessionId": "$session_id",
    "ppid": int("$session_id".split('_')[1]),
    "startTime": datetime.now().isoformat(),
    "endTime": None,
    "status": "inactive",
    "workingDirectory": "/tmp",
    "gitBranch": "main",
    "activeMode": "Default Mode",
    "sessionContext": "Recovered session",
    "lastActivity": datetime.now().isoformat(),
    "commandCount": 0,
    "memoryEntities": [],
    "todoItems": [],
    "artifacts": []
}

with open('$metadata_file', 'w') as f:
    json.dump(metadata, f, indent=2)
EOF
            echo "REPAIRED: Created default metadata"
            ((repairs_made++))
        fi
    fi
    
    # Check and repair state
    local state_file="$session_dir/state.json"
    if [[ ! -f "$state_file" ]]; then
        echo "REPAIR NEEDED: State file missing"
        ((repairs_needed++))
        
        if [[ "$dry_run" == "false" ]]; then
            source "$SCRIPT_DIR/session_state.sh"
            initialize_session_state "$session_id" "$base_dir" >/dev/null
            echo "REPAIRED: Created default state"
            ((repairs_made++))
        fi
    fi
    
    echo "=== Repair Summary ==="
    echo "Repairs needed: $repairs_needed"
    
    if [[ "$dry_run" == "true" ]]; then
        echo "DRY RUN: No repairs performed"
        echo "Run with dry_run=false to perform repairs"
    else
        echo "Repairs made: $repairs_made"
        if [[ $repairs_made -eq $repairs_needed ]]; then
            echo "✓ All repairs completed successfully"
        else
            echo "✗ Some repairs failed"
        fi
    fi
    
    return 0
}

# Print help
print_validation_help() {
    cat << EOF
FlowLoom Session Validation

Usage: session_validation.sh <command> [options]

Commands:
  metadata <metadata_file> [strict]           Validate session metadata JSON
  state <state_file> [strict]                 Validate session state JSON
  index <index_file> [strict]                 Validate session index JSON
  backup <manifest_file> [strict]             Validate backup manifest JSON
  session <session_id> [base_dir] [strict]    Validate complete session
  repair <session_id> [base_dir] [dry_run]    Repair session structure issues

Parameters:
  strict: true/false - Enable strict validation mode (default: false)
  dry_run: true/false - Only analyze, don't perform repairs (default: true)

Examples:
  session_validation.sh metadata /path/to/metadata.json
  session_validation.sh state /path/to/state.json true
  session_validation.sh session session_123_456_abc123
  session_validation.sh repair session_123_456_abc123 .flowloom false

Validation Modes:
  Normal: Validates required fields and basic structure
  Strict: Additional validations for data consistency and best practices

Exit Codes:
  0: Validation passed
  1: Validation failed
  2: File not found or invalid arguments

Notes:
  - Validates JSON structure and FlowLoom-specific requirements
  - Strict mode performs additional consistency checks
  - Repair mode can fix common structural issues
  - Respects Claude Code security boundaries
EOF
}

# Main command dispatcher
main() {
    if [[ $# -eq 0 ]]; then
        print_validation_help
        exit 1
    fi
    
    local command="$1"
    shift
    
    case "$command" in
        "metadata")
            if [[ $# -lt 1 ]]; then
                echo "Error: metadata validation requires metadata_file" >&2
                exit 2
            fi
            validate_session_metadata "$@"
            ;;
        "state")
            if [[ $# -lt 1 ]]; then
                echo "Error: state validation requires state_file" >&2
                exit 2
            fi
            validate_session_state "$@"
            ;;
        "index")
            if [[ $# -lt 1 ]]; then
                echo "Error: index validation requires index_file" >&2
                exit 2
            fi
            validate_session_index "$@"
            ;;
        "backup")
            if [[ $# -lt 1 ]]; then
                echo "Error: backup validation requires manifest_file" >&2
                exit 2
            fi
            validate_backup_manifest "$@"
            ;;
        "session")
            if [[ $# -lt 1 ]]; then
                echo "Error: session validation requires session_id" >&2
                exit 2
            fi
            validate_complete_session "$@"
            ;;
        "repair")
            if [[ $# -lt 1 ]]; then
                echo "Error: repair requires session_id" >&2
                exit 2
            fi
            repair_session "$@"
            ;;
        "help"|"--help"|"-h")
            print_validation_help
            ;;
        *)
            echo "Error: Unknown command: $command" >&2
            echo "Use 'session_validation.sh help' for usage information" >&2
            exit 2
            ;;
    esac
}

# Run main function if script is executed directly
if [[ "${BASH_SOURCE[0]:-$0}" == "${0}" ]]; then
    main "$@"
fi