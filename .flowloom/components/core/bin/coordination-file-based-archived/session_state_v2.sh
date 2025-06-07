#!/bin/bash
# Session State V2 - Unified state model with coordination support
set -euo pipefail

# Source existing utilities if available
if [[ -f "${FLOWLOOM_DIR:-$HOME/.flowloom}/bin/flowloom_utils.sh" ]]; then
    source "${FLOWLOOM_DIR:-$HOME/.flowloom}/bin/flowloom_utils.sh"
fi

# Initialize state with coordination support
init_session_state_v2() {
    local session_id="$1"
    local shell_pid="$2"
    local preferred_role="${3:-implementer}"
    
    local state_file="${FLOWLOOM_DIR}/sessions/${session_id}/state.json"
    
    # Create state directory if needed
    mkdir -p "$(dirname "$state_file")"
    
    # Create initial state with coordination using Python
    python3 -c "
import json
from datetime import datetime

state = {
    'version': 1,
    'sessionId': '$session_id',
    'shellPid': $shell_pid,
    'created': datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ'),
    'lastActivity': datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ'),
    'coordination': {
        'enabled': True,
        'capabilities': {
            'can_review': False,
            'can_implement': False,
            'can_coordinate': False,
            'can_test': False,
            'can_document': False,
            'can_analyze': False
        },
        'preferred_role': '$preferred_role',
        'workflow': None,
        'partnerships': []
    },
    'tasks': {
        'current': None,
        'queue': [],
        'completed': []
    },
    'audit': {
        'events': [
            {
                'timestamp': datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ'),
                'event': 'session_created',
                'details': 'Session initialized with coordination support'
            }
        ]
    }
}

with open('$state_file', 'w') as f:
    json.dump(state, f, indent=2)
"
    
    # Set secure permissions
    chmod 600 "$state_file"
    
    echo "$state_file"
}

# Update capabilities for a session
set_session_capabilities() {
    local session_id="$1"
    shift
    local capabilities=("$@")
    
    local state_file="${FLOWLOOM_DIR}/sessions/${session_id}/state.json"
    
    # Convert bash array to Python list
    local caps_str=""
    for cap in "${capabilities[@]}"; do
        caps_str="${caps_str}'${cap}',"
    done
    caps_str="[${caps_str%,}]"
    
    # Update state file using Python
    python3 -c "
import json
from datetime import datetime

with open('$state_file', 'r') as f:
    state = json.load(f)

capabilities = $caps_str
for cap in capabilities:
    state['coordination']['capabilities'][f'can_{cap}'] = True

state['lastActivity'] = datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ')

with open('$state_file', 'w') as f:
    json.dump(state, f, indent=2)
"
}

# Get session state
get_session_state_v2() {
    local session_id="$1"
    local state_file="${FLOWLOOM_DIR}/sessions/${session_id}/state.json"
    
    if [[ -f "$state_file" ]]; then
        cat "$state_file"
    else
        echo "{}" >&2
        return 1
    fi
}

# Add audit event
add_audit_event() {
    local session_id="$1"
    local event_type="$2"
    local details="$3"
    
    local state_file="${FLOWLOOM_DIR}/sessions/${session_id}/state.json"
    
    python3 -c "
import json
from datetime import datetime

with open('$state_file', 'r') as f:
    state = json.load(f)

event = {
    'timestamp': datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ'),
    'event': '$event_type',
    'details': '$details'
}

state['audit']['events'].append(event)
state['lastActivity'] = datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ')

with open('$state_file', 'w') as f:
    json.dump(state, f, indent=2)
"
}

# Export functions
export -f init_session_state_v2
export -f set_session_capabilities
export -f get_session_state_v2
export -f add_audit_event