#!/bin/bash
# Coordination Locking - Atomic state operations
set -euo pipefail

# Lock timeout in seconds
DEFAULT_LOCK_TIMEOUT=30

# Acquire exclusive lock for state operations
acquire_state_lock() {
    local session_id="$1"
    local timeout="${2:-$DEFAULT_LOCK_TIMEOUT}"
    
    local lock_file="${FLOWLOOM_DIR}/sessions/${session_id}/.state.lock"
    local lock_dir=$(dirname "$lock_file")
    
    # Ensure lock directory exists
    mkdir -p "$lock_dir"
    
    # Try to acquire lock with timeout
    local lock_acquired=false
    local start_time=$(date +%s)
    
    while [[ $(($(date +%s) - start_time)) -lt $timeout ]]; do
        if mkdir "${lock_file}.lck" 2>/dev/null; then
            lock_acquired=true
            # Store lock info
            echo "$$:$(date -u +%Y-%m-%dT%H:%M:%SZ)" > "${lock_file}.lck/info"
            break
        fi
        sleep 0.1
    done
    
    if [[ "$lock_acquired" != "true" ]]; then
        echo "ERROR: Failed to acquire lock for session $session_id after ${timeout}s" >&2
        return 1
    fi
    
    return 0
}

# Release state lock
release_state_lock() {
    local session_id="$1"
    local lock_file="${FLOWLOOM_DIR}/sessions/${session_id}/.state.lock"
    
    # Remove lock directory
    rm -rf "${lock_file}.lck" 2>/dev/null || true
}

# Execute function with state lock
with_state_lock() {
    local session_id="$1"
    local function_name="$2"
    shift 2
    local args=("$@")
    
    # Acquire lock
    if ! acquire_state_lock "$session_id"; then
        return 1
    fi
    
    # Execute function
    local result=0
    "$function_name" "$session_id" "${args[@]}" || result=$?
    
    # Always release lock
    release_state_lock "$session_id"
    
    return $result
}

# Atomic state update using Python
update_state_atomic() {
    local session_id="$1"
    local update_description="$2"
    shift 2
    local update_args=("$@")
    
    local state_file="${FLOWLOOM_DIR}/sessions/${session_id}/state.json"
    
    if [[ ! -f "$state_file" ]]; then
        echo "ERROR: State file not found for session $session_id" >&2
        return 1
    fi
    
    # Convert bash array to Python list for additional args
    local args_str=""
    for arg in "${update_args[@]}"; do
        args_str="${args_str}'${arg}',"
    done
    args_str="[${args_str%,}]"
    
    # Update state file using Python
    python3 -c "
import json
from datetime import datetime

with open('$state_file', 'r') as f:
    state = json.load(f)

# Apply specific updates based on description
update_type = '$update_description'
args = $args_str

if update_type == 'set_task':
    task_id, task_desc = args[0], args[1]
    state['tasks']['current'] = {'id': task_id, 'description': task_desc}
elif update_type == 'complete_task':
    if state['tasks']['current']:
        state['tasks']['completed'].append(state['tasks']['current'])
        state['tasks']['current'] = None
elif update_type == 'add_queue_task':
    task_data = json.loads(args[0]) if args else {}
    state['tasks']['queue'].append(task_data)

# Always update timestamp
state['lastActivity'] = datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ')

with open('$state_file', 'w') as f:
    json.dump(state, f, indent=2)
"
    
    # Maintain permissions
    chmod 600 "$state_file"
}

# Atomic registry update
update_registry_atomic() {
    local update_description="$1"
    shift
    local update_args=("$@")
    
    local registry_file="${FLOWLOOM_DIR}/coordination/registry.json"
    local lock_file="${FLOWLOOM_DIR}/coordination/.registry.lock"
    
    # Simple file-based lock for registry
    local lock_acquired=false
    local max_attempts=100
    local attempt=0
    
    while [[ $attempt -lt $max_attempts ]]; do
        if mkdir "${lock_file}.lck" 2>/dev/null; then
            lock_acquired=true
            break
        fi
        sleep 0.05
        ((attempt++))
    done
    
    if [[ "$lock_acquired" != "true" ]]; then
        echo "ERROR: Failed to acquire registry lock" >&2
        return 1
    fi
    
    # Convert bash array to Python list
    local args_str=""
    for arg in "${update_args[@]}"; do
        args_str="${args_str}'${arg}',"
    done
    args_str="[${args_str%,}]"
    
    # Update registry using Python
    python3 -c "
import json
import os
from datetime import datetime

if not os.path.exists('$registry_file'):
    exit(0)

with open('$registry_file', 'r') as f:
    registry = json.load(f)

# Apply updates based on description
update_type = '$update_description'
args = $args_str

if update_type == 'update_session_status':
    session_id, status = args[0], args[1]
    if session_id in registry['sessions']:
        registry['sessions'][session_id]['status'] = status

registry['last_updated'] = datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ')

with open('$registry_file', 'w') as f:
    json.dump(registry, f, indent=2)
"
    
    chmod 600 "$registry_file"
    
    # Release lock
    rm -rf "${lock_file}.lck"
}

# Check if lock is held
is_locked() {
    local session_id="$1"
    local lock_file="${FLOWLOOM_DIR}/sessions/${session_id}/.state.lock"
    
    [[ -d "${lock_file}.lck" ]]
}

# Get lock info
get_lock_info() {
    local session_id="$1"
    local lock_file="${FLOWLOOM_DIR}/sessions/${session_id}/.state.lock"
    
    if [[ -f "${lock_file}.lck/info" ]]; then
        cat "${lock_file}.lck/info"
    else
        echo "No lock"
    fi
}

# Clean stale locks (for recovery)
clean_stale_locks() {
    local max_age_seconds="${1:-300}"  # 5 minutes default
    
    find "${FLOWLOOM_DIR}/sessions" -name ".state.lock.lck" -type d -mmin +$((max_age_seconds / 60)) -exec rm -rf {} \; 2>/dev/null || true
    find "${FLOWLOOM_DIR}/coordination" -name ".registry.lock.lck" -type d -mmin +$((max_age_seconds / 60)) -exec rm -rf {} \; 2>/dev/null || true
}

# Export functions
export -f acquire_state_lock
export -f release_state_lock
export -f with_state_lock
export -f update_state_atomic
export -f update_registry_atomic
export -f is_locked
export -f get_lock_info
export -f clean_stale_locks