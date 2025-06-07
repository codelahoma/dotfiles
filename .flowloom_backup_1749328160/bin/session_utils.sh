#!/bin/bash
# bin/session_utils.sh - Common session utilities and error handling
# Part of FlowLoom Session Management System
# Based on Plan 3120 - Session Tracking Detailed Design
#
# SECURITY & ETHICS NOTICE:
# This utility script provides common functions for session management while
# respecting Claude Code's security boundaries. All operations are designed
# to work within approved workflows and should never be used to circumvent
# security measures or access unauthorized resources.

set -euo pipefail

# Mark this script as loaded to prevent circular dependencies (only if not already set)
if [[ -z "${SESSION_UTILS_LOADED:-}" ]]; then
    readonly SESSION_UTILS_LOADED=1
fi

# Color codes for output formatting (only define if not already defined)
if [[ -z "${RED:-}" ]]; then
    readonly RED='\033[0;31m'
    readonly GREEN='\033[0;32m'
    readonly YELLOW='\033[1;33m'
    readonly BLUE='\033[0;34m'
    readonly PURPLE='\033[0;35m'
    readonly CYAN='\033[0;36m'
    readonly NC='\033[0m' # No Color
fi

# Session management constants (only define if not already defined)
if [[ -z "${SESSION_DEFAULT_BASE_DIR:-}" ]]; then
    readonly SESSION_DEFAULT_BASE_DIR="${FLOWLOOM_WORK_DIR:-.flowloom}"
    readonly SESSION_LOG_FILE="${SESSION_DEFAULT_BASE_DIR}/session.log"
    readonly SESSION_VERSION="1.0.0"
    
    # Logging levels
    readonly LOG_DEBUG=0
    readonly LOG_INFO=1
    readonly LOG_WARN=2
    readonly LOG_ERROR=3
    readonly LOG_FATAL=4
fi

# Current log level (can be overridden by environment)
SESSION_LOG_LEVEL="${SESSION_LOG_LEVEL:-$LOG_INFO}"

# Initialize logging
init_session_logging() {
    local base_dir="${1:-$SESSION_DEFAULT_BASE_DIR}"
    local log_file="$base_dir/session.log"
    
    # Create log directory if it doesn't exist
    mkdir -p "$(dirname "$log_file")"
    
    # Create log file if it doesn't exist
    if [[ ! -f "$log_file" ]]; then
        echo "$(date -Iseconds) [INFO] Session logging initialized - FlowLoom Session Management v$SESSION_VERSION" > "$log_file"
        echo "$(date -Iseconds) [INFO] Security: Operating within Claude Code approved boundaries" >> "$log_file"
    fi
}

# Log message with level
session_log_with_level() {
    local level="$1"
    local level_num="$2"
    local message="$3"
    local base_dir="${4:-$SESSION_DEFAULT_BASE_DIR}"
    
    # Only log if level is at or above current log level
    if [[ $level_num -ge $SESSION_LOG_LEVEL ]]; then
        local log_file="$base_dir/session.log"
        local timestamp
        timestamp=$(date -Iseconds)
        
        # Ensure log directory exists
        mkdir -p "$(dirname "$log_file")"
        
        echo "$timestamp [$level] $message" >> "$log_file"
        
        # Also output to stderr for ERROR and FATAL
        if [[ $level_num -ge $LOG_ERROR ]]; then
            echo -e "${RED}[$level]${NC} $message" >&2
        elif [[ $level_num -eq $LOG_WARN ]]; then
            echo -e "${YELLOW}[$level]${NC} $message" >&2
        fi
    fi
}

# Convenience logging functions
session_debug() {
    session_log_with_level "DEBUG" $LOG_DEBUG "$1" "${2:-}"
}

session_log() {
    session_log_with_level "INFO" $LOG_INFO "$1" "${2:-}"
}

session_warn() {
    session_log_with_level "WARN" $LOG_WARN "$1" "${2:-}"
}

session_error() {
    session_log_with_level "ERROR" $LOG_ERROR "$1" "${2:-}"
}

session_fatal() {
    session_log_with_level "FATAL" $LOG_FATAL "$1" "${2:-}"
    exit 1
}

# Colored output functions
session_success() {
    echo -e "${GREEN}✅ $1${NC}"
    session_log "SUCCESS: $1"
}

session_info() {
    echo -e "${CYAN}ℹ️  $1${NC}"
    session_log "INFO: $1"
}

session_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
    session_warn "$1"
}

session_failure() {
    echo -e "${RED}❌ $1${NC}"
    session_error "$1"
}

# Validate session ID format (used by multiple scripts)
validate_session_id() {
    local session_id="$1"
    # New format: PPID-purpose (e.g., 58883-session-management)
    if [[ "$session_id" =~ ^[0-9]+-[a-z0-9]([a-z0-9-]*[a-z0-9])?$ ]]; then
        return 0
    # Legacy format: PPID-6char (e.g., 58883-abc123)
    elif [[ "$session_id" =~ ^[0-9]+-[a-z0-9][a-z0-9][a-z0-9][a-z0-9][a-z0-9][a-z0-9]$ ]]; then
        return 0
    else
        session_error "Invalid session ID format: $session_id"
        return 1
    fi
}

# Check if required dependencies are available
check_session_dependencies() {
    local missing_deps=()
    
    # Check for required commands
    local required_commands=("python3" "jq" "date" "find" "grep" "sed")
    
    for cmd in "${required_commands[@]}"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            missing_deps+=("$cmd")
        fi
    done
    
    if [[ ${#missing_deps[@]} -gt 0 ]]; then
        session_error "Missing required dependencies: ${missing_deps[*]}"
        return 1
    fi
    
    return 0
}

# Safe file operations with backup
safe_write_file() {
    local file_path="$1"
    local content="$2"
    local backup_suffix="${3:-.backup}"
    
    # Create backup if file exists
    if [[ -f "$file_path" ]]; then
        cp "$file_path" "${file_path}${backup_suffix}"
        session_debug "Created backup: ${file_path}${backup_suffix}"
    fi
    
    # Write to temporary file first
    local temp_file="${file_path}.tmp.$$"
    echo "$content" > "$temp_file"
    
    # Atomic move
    mv "$temp_file" "$file_path"
    session_debug "Safely wrote file: $file_path"
}

# JSON validation using Python
validate_json_file() {
    local json_file="$1"
    
    if [[ ! -f "$json_file" ]]; then
        session_error "JSON file not found: $json_file"
        return 1
    fi
    
    python3 -c "
import json
import sys

try:
    with open('$json_file', 'r') as f:
        json.load(f)
    sys.exit(0)
except json.JSONDecodeError as e:
    print(f'Invalid JSON in $json_file: {e}', file=sys.stderr)
    sys.exit(1)
except Exception as e:
    print(f'Error reading $json_file: {e}', file=sys.stderr)
    sys.exit(1)
"
}

# Get JSON field value using Python (more robust than jq for our use case)
get_json_field() {
    local json_file="$1"
    local field_path="$2"
    local default_value="${3:-}"
    
    python3 -c "
import json
import sys

try:
    with open('$json_file', 'r') as f:
        data = json.load(f)
    
    # Simple field access (extend for nested paths if needed)
    value = data.get('$field_path', '$default_value')
    if value is None and '$default_value':
        value = '$default_value'
    
    if value is None:
        print('null')
    else:
        print(value)
        
except (json.JSONDecodeError, FileNotFoundError, KeyError) as e:
    if '$default_value':
        print('$default_value')
    else:
        print(f'Error getting field $field_path from $json_file: {e}', file=sys.stderr)
        sys.exit(1)
"
}

# Update JSON field using Python
update_json_field() {
    local json_file="$1"
    local field_path="$2"
    local new_value="$3"
    
    python3 -c "
import json
import sys
import os
from datetime import datetime

try:
    # Read existing data
    if os.path.exists('$json_file'):
        with open('$json_file', 'r') as f:
            data = json.load(f)
    else:
        data = {}
    
    # Handle different value types
    if '$new_value' == 'null':
        data['$field_path'] = None
    elif '$new_value'.isdigit():
        data['$field_path'] = int('$new_value')
    elif '$new_value' in ['true', 'false']:
        data['$field_path'] = '$new_value' == 'true'
    else:
        data['$field_path'] = '$new_value'
    
    # Update lastActivity for metadata files
    if 'lastActivity' in data:
        data['lastActivity'] = datetime.now().isoformat()
    
    # Write back
    with open('$json_file', 'w') as f:
        json.dump(data, f, indent=2)
        
except Exception as e:
    print(f'Error updating JSON field $field_path in $json_file: {e}', file=sys.stderr)
    sys.exit(1)
"
}

# Create timestamped backup of file
create_timestamped_backup() {
    local file_path="$1"
    local backup_dir="${2:-$(dirname "$file_path")/backups}"
    
    if [[ ! -f "$file_path" ]]; then
        session_warn "Cannot backup non-existent file: $file_path"
        return 1
    fi
    
    mkdir -p "$backup_dir"
    
    local filename
    filename=$(basename "$file_path")
    local timestamp
    timestamp=$(date +%Y%m%d_%H%M%S)
    local backup_file="$backup_dir/${filename}.${timestamp}.backup"
    
    cp "$file_path" "$backup_file"
    session_debug "Created timestamped backup: $backup_file"
    echo "$backup_file"
}

# Clean up old backup files
cleanup_old_backups() {
    local backup_dir="$1"
    local max_age_days="${2:-7}"
    local max_count="${3:-10}"
    
    if [[ ! -d "$backup_dir" ]]; then
        return 0
    fi
    
    session_debug "Cleaning up backups older than $max_age_days days in $backup_dir"
    
    # Remove files older than max_age_days
    find "$backup_dir" -name "*.backup" -type f -mtime "+$max_age_days" -delete
    
    # Keep only the most recent max_count files
    local backup_files
    mapfile -t backup_files < <(find "$backup_dir" -name "*.backup" -type f -printf '%T@ %p\n' | sort -rn | cut -d' ' -f2-)
    
    if [[ ${#backup_files[@]} -gt $max_count ]]; then
        local files_to_remove=("${backup_files[@]:$max_count}")
        rm -f "${files_to_remove[@]}"
        session_debug "Removed $((${#backup_files[@]} - max_count)) old backup files"
    fi
}

# Progress indicator for long operations
show_progress() {
    local current="$1"
    local total="$2"
    local description="${3:-Processing}"
    
    local percentage=$((current * 100 / total))
    local completed=$((percentage / 2))
    local remaining=$((50 - completed))
    
    printf "\r$description: ["
    printf "%${completed}s" | tr ' ' '='
    printf "%${remaining}s" | tr ' ' '-'
    printf "] %d%% (%d/%d)" "$percentage" "$current" "$total"
}

# Finish progress indicator
finish_progress() {
    echo ""
}

# Session environment validation
validate_session_environment() {
    local base_dir="${1:-$SESSION_DEFAULT_BASE_DIR}"
    
    session_debug "Validating session environment in $base_dir"
    
    # Check dependencies
    if ! check_session_dependencies; then
        return 1
    fi
    
    # Check write permissions
    if [[ ! -w "$(dirname "$base_dir")" ]]; then
        session_error "No write permission for session directory parent: $(dirname "$base_dir")"
        return 1
    fi
    
    # Initialize logging
    init_session_logging "$base_dir"
    
    session_debug "Session environment validation completed successfully"
    return 0
}

# Main execution for testing
if [[ "${BASH_SOURCE[0]:-$0}" == "${0}" ]]; then
    case "${1:-}" in
        "validate-env")
            if validate_session_environment "${2:-}"; then
                session_success "Session environment is valid"
                exit 0
            else
                session_failure "Session environment validation failed"
                exit 1
            fi
            ;;
        "test-logging")
            init_session_logging "${2:-.flowloom}"
            session_debug "This is a debug message"
            session_log "This is an info message"
            session_warn "This is a warning message"
            session_error "This is an error message"
            session_success "Logging test completed"
            ;;
        "test-json")
            if [[ -z "${2:-}" ]]; then
                echo "Usage: $0 test-json <json_file>" >&2
                exit 1
            fi
            if validate_json_file "$2"; then
                session_success "JSON file is valid: $2"
            else
                session_failure "JSON file is invalid: $2"
                exit 1
            fi
            ;;
        "deps")
            if check_session_dependencies; then
                session_success "All session dependencies are available"
                exit 0
            else
                session_failure "Missing session dependencies"
                exit 1
            fi
            ;;
        "test")
            echo "Testing session utilities..."
            
            # Test environment validation
            echo "Testing environment validation..."
            validate_session_environment "/tmp/test_session_utils"
            
            # Test logging
            echo "Testing logging functions..."
            session_debug "Debug test"
            session_log "Info test"
            session_warn "Warning test"
            session_success "Success test"
            
            echo "Session utilities test completed successfully"
            ;;
        *)
            echo "FlowLoom Session Utilities"
            echo "Usage: $0 {validate-env|test-logging|test-json|deps|test} [args]"
            echo ""
            echo "Commands:"
            echo "  validate-env [base_dir]  - Validate session environment"
            echo "  test-logging [base_dir]  - Test logging functions"
            echo "  test-json <json_file>    - Validate JSON file"
            echo "  deps                     - Check dependencies"
            echo "  test                     - Run basic functionality test"
            echo ""
            echo "This script provides common utilities for FlowLoom session management."
            echo "It respects Claude Code security boundaries and operates within approved workflows."
            exit 1
            ;;
    esac
fi