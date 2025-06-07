#!/bin/bash
# Session Registry - Efficient session discovery and indexing
set -euo pipefail

# Registry file location
REGISTRY_FILE="${FLOWLOOM_DIR:-$HOME/.flowloom}/coordination/registry.json"

# Initialize registry
init_registry() {
    mkdir -p "$(dirname "$REGISTRY_FILE")"
    
    if [[ ! -f "$REGISTRY_FILE" ]]; then
        python3 -c "
import json
from datetime import datetime

registry = {
    'version': 1,
    'last_updated': datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ'),
    'index': {
        'by_role': {},
        'by_capability': {},
        'by_workflow': {}
    },
    'sessions': {}
}

with open('$REGISTRY_FILE', 'w') as f:
    json.dump(registry, f, indent=2)
"
        chmod 600 "$REGISTRY_FILE"
    fi
}

# Register session in registry
register_session() {
    local session_id="$1"
    local session_path="$2"
    
    init_registry
    
    python3 -c "
import json
from datetime import datetime

# Load session state
try:
    with open('${session_path}/state.json', 'r') as f:
        state = json.load(f)
    preferred_role = state.get('coordination', {}).get('preferred_role', 'unknown')
except:
    preferred_role = 'unknown'

# Update registry
with open('$REGISTRY_FILE', 'r') as f:
    registry = json.load(f)

registry['sessions']['$session_id'] = {
    'endpoint': '$session_path',
    'status': 'active',
    'last_heartbeat': datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ')
}
registry['last_updated'] = datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ')

with open('$REGISTRY_FILE', 'w') as f:
    json.dump(registry, f, indent=2)
"
    
    # Update indices
    update_registry_indices "$session_id"
}

# Update registry indices for a session
update_registry_indices() {
    local session_id="$1"
    
    python3 -c "
import json

# Load registry
with open('$REGISTRY_FILE', 'r') as f:
    registry = json.load(f)

# Get session info
session_path = registry['sessions'].get('$session_id', {}).get('endpoint')
if not session_path:
    exit(1)

# Load session state
try:
    with open(f'{session_path}/state.json', 'r') as f:
        state = json.load(f)
except:
    exit(1)

# Extract indexable attributes
coord = state.get('coordination', {})
preferred_role = coord.get('preferred_role', 'unknown')
capabilities = [k for k, v in coord.get('capabilities', {}).items() if v]
workflow_id = coord.get('workflow', {}).get('template_id') if coord.get('workflow') else None

# Update role index
if preferred_role not in registry['index']['by_role']:
    registry['index']['by_role'][preferred_role] = []
if '$session_id' not in registry['index']['by_role'][preferred_role]:
    registry['index']['by_role'][preferred_role].append('$session_id')

# Update capability indices
for cap in capabilities:
    if cap not in registry['index']['by_capability']:
        registry['index']['by_capability'][cap] = []
    if '$session_id' not in registry['index']['by_capability'][cap]:
        registry['index']['by_capability'][cap].append('$session_id')

# Update workflow index if applicable
if workflow_id:
    if workflow_id not in registry['index']['by_workflow']:
        registry['index']['by_workflow'][workflow_id] = []
    if '$session_id' not in registry['index']['by_workflow'][workflow_id]:
        registry['index']['by_workflow'][workflow_id].append('$session_id')

# Save registry
with open('$REGISTRY_FILE', 'w') as f:
    json.dump(registry, f, indent=2)
"
}

# Remove session from registry
unregister_session() {
    local session_id="$1"
    
    python3 -c "
import json
from datetime import datetime

with open('$REGISTRY_FILE', 'r') as f:
    registry = json.load(f)

# Remove from all indices
for role, sessions in registry['index']['by_role'].items():
    if '$session_id' in sessions:
        sessions.remove('$session_id')

for cap, sessions in registry['index']['by_capability'].items():
    if '$session_id' in sessions:
        sessions.remove('$session_id')

for workflow, sessions in registry['index']['by_workflow'].items():
    if '$session_id' in sessions:
        sessions.remove('$session_id')

# Remove from sessions
if '$session_id' in registry['sessions']:
    del registry['sessions']['$session_id']

registry['last_updated'] = datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ')

with open('$REGISTRY_FILE', 'w') as f:
    json.dump(registry, f, indent=2)
"
}

# Find sessions by capability
find_by_capability() {
    local capability="$1"
    
    python3 -c "
import json

try:
    with open('$REGISTRY_FILE', 'r') as f:
        registry = json.load(f)
    
    sessions = registry['index']['by_capability'].get('$capability', [])
    for session in sessions:
        print(session)
except:
    pass
"
}

# Find sessions by role
find_by_role() {
    local role="$1"
    
    python3 -c "
import json

try:
    with open('$REGISTRY_FILE', 'r') as f:
        registry = json.load(f)
    
    sessions = registry['index']['by_role'].get('$role', [])
    for session in sessions:
        print(session)
except:
    pass
"
}

# Get all active sessions
get_active_sessions() {
    python3 -c "
import json

try:
    with open('$REGISTRY_FILE', 'r') as f:
        registry = json.load(f)
    
    for session_id, info in registry['sessions'].items():
        if info.get('status') == 'active':
            print(session_id)
except:
    pass
"
}

# Update session heartbeat
update_heartbeat() {
    local session_id="$1"
    
    python3 -c "
import json
from datetime import datetime

with open('$REGISTRY_FILE', 'r') as f:
    registry = json.load(f)

if '$session_id' in registry['sessions']:
    registry['sessions']['$session_id']['last_heartbeat'] = datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ')

with open('$REGISTRY_FILE', 'w') as f:
    json.dump(registry, f, indent=2)
"
}

# Export functions
export -f init_registry
export -f register_session
export -f unregister_session
export -f find_by_capability
export -f find_by_role
export -f get_active_sessions
export -f update_heartbeat