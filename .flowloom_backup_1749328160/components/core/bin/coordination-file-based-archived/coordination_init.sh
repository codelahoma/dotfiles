#!/bin/bash
# Coordination System Initialization
set -euo pipefail

# Initialize coordination infrastructure
init_coordination_system() {
    echo "Initializing FlowLoom Coordination System..."
    
    # Source all coordination modules
    source "${FLOWLOOM_DIR}/bin/session_state_v2.sh"
    source "${FLOWLOOM_DIR}/bin/session_registry.sh"
    source "${FLOWLOOM_DIR}/bin/coordination_locking.sh"
    source "${FLOWLOOM_DIR}/bin/coordination_audit.sh"
    
    # Initialize components
    init_registry
    init_audit_system
    
    # Clean any stale locks
    clean_stale_locks
    
    # Log initialization
    log_coordination_event "system_init" "system" "Coordination system initialized" "INFO"
    
    echo "Coordination system initialized successfully"
}

# Upgrade existing session to v2
upgrade_session_to_v2() {
    local session_id="$1"
    local old_state_file="${FLOWLOOM_DIR}/sessions/${session_id}/state.json"
    
    if [[ ! -f "$old_state_file" ]]; then
        echo "Session $session_id not found" >&2
        return 1
    fi
    
    # Check if already v2 using Python
    local is_v2=$(python3 -c "
import json

try:
    with open('$old_state_file', 'r') as f:
        state = json.load(f)
    
    version = state.get('version', 0)
    has_coordination = 'coordination' in state
    
    if version == 1 and has_coordination:
        print('true')
    else:
        print('false')
except:
    print('false')
")
    
    if [[ "$is_v2" == "true" ]]; then
        echo "Session $session_id already at version 2"
        return 0
    fi
    
    # Backup old state
    cp "$old_state_file" "${old_state_file}.pre-v2.bak"
    
    # Extract old data and create new v2 state using Python
    python3 -c "
import json
from datetime import datetime

# Read old state
try:
    with open('$old_state_file', 'r') as f:
        old_state = json.load(f)
except:
    old_state = {}

# Extract relevant data
shell_pid = old_state.get('shellPid', old_state.get('shellPID', 0))
created = old_state.get('created', datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ'))
old_tasks = old_state.get('tasks', {'current': None, 'queue': [], 'completed': []})

# Create new v2 state
new_state = {
    'version': 1,
    'sessionId': '$session_id',
    'shellPid': shell_pid,
    'created': created,
    'lastActivity': datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ'),
    'coordination': {
        'enabled': True,
        'capabilities': {
            'can_review': False,
            'can_implement': True,  # Default implementer capabilities
            'can_coordinate': False,
            'can_test': False,
            'can_document': False,
            'can_analyze': False
        },
        'preferred_role': 'implementer',
        'workflow': None,
        'partnerships': []
    },
    'tasks': old_tasks,
    'audit': {
        'events': [
            {
                'timestamp': datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ'),
                'event': 'session_upgraded',
                'details': 'Upgraded session to v2 with coordination support'
            }
        ]
    }
}

# Merge any existing audit events
if 'audit' in old_state and 'events' in old_state['audit']:
    new_state['audit']['events'] = old_state['audit']['events'] + new_state['audit']['events']

with open('$old_state_file', 'w') as f:
    json.dump(new_state, f, indent=2)
"
    
    # Set correct permissions
    chmod 600 "$old_state_file"
    
    # Register in registry
    register_session "$session_id" "${FLOWLOOM_DIR}/sessions/${session_id}"
    
    # Log upgrade
    log_coordination_event "session_upgraded" "$session_id" "Upgraded session to v2" "INFO"
    
    echo "Session $session_id upgraded to v2"
}

# Health check
coordination_health_check() {
    echo "=== Coordination System Health Check ==="
    
    # Check registry
    if [[ -f "${FLOWLOOM_DIR}/coordination/registry.json" ]]; then
        echo "✓ Registry exists"
        local session_count=$(python3 -c "
import json
try:
    with open('${FLOWLOOM_DIR}/coordination/registry.json', 'r') as f:
        registry = json.load(f)
    print(len(registry.get('sessions', {})))
except:
    print(0)
")
        echo "  Active sessions: $session_count"
    else
        echo "✗ Registry not found"
    fi
    
    # Check audit system
    if [[ -d "${FLOWLOOM_DIR}/audit" ]]; then
        echo "✓ Audit system initialized"
        local today_log="${FLOWLOOM_DIR}/audit/audit-$(date +%Y%m%d).log"
        if [[ -f "$today_log" ]]; then
            local event_count=$(wc -l < "$today_log")
            echo "  Today's events: $event_count"
        fi
    else
        echo "✗ Audit system not initialized"
    fi
    
    # Check for stale locks
    local stale_locks=$(find "${FLOWLOOM_DIR}" -name "*.lock.lck" -type d -mmin +5 2>/dev/null | wc -l)
    if [[ $stale_locks -eq 0 ]]; then
        echo "✓ No stale locks found"
    else
        echo "⚠ Found $stale_locks stale locks (older than 5 minutes)"
    fi
}

# Main execution
case "${1:-init}" in
    "init")
        init_coordination_system
        ;;
    "upgrade")
        upgrade_session_to_v2 "${2:-}"
        ;;
    "health")
        coordination_health_check
        ;;
    *)
        echo "Usage: $0 {init|upgrade <session-id>|health}"
        exit 1
        ;;
esac