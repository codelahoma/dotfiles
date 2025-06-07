#!/bin/bash
# Capability-Based Coordination Logic
set -euo pipefail

# Source dependencies
source "${FLOWLOOM_DIR:-$HOME/.flowloom}/bin/session_state_v2.sh"
source "${FLOWLOOM_DIR:-$HOME/.flowloom}/bin/session_registry.sh"
source "${FLOWLOOM_DIR:-$HOME/.flowloom}/bin/coordination_locking.sh"

# Capability definitions and mappings
declare -A CAPABILITY_DESCRIPTIONS=(
    ["can_implement"]="Write and modify code"
    ["can_review"]="Review code and provide feedback"
    ["can_test"]="Create and run tests"
    ["can_document"]="Write documentation"
    ["can_coordinate"]="Manage workflow and assignments"
    ["can_analyze"]="Perform analysis and research"
)

# Role to capabilities mapping
declare -A ROLE_CAPABILITIES=(
    ["implementer"]="implement,test"
    ["reviewer"]="review,analyze"
    ["tester"]="test,analyze"
    ["documenter"]="document,analyze"
    ["coordinator"]="coordinate,review,analyze"
    ["analyst"]="analyze,document"
)

# Check if session has capability
session_has_capability() {
    local session_id="$1"
    local capability="$2"
    
    local has_cap=$(python3 -c "
import json
import sys

try:
    state = json.loads(sys.stdin.read())
    cap_value = state.get('coordination', {}).get('capabilities', {}).get('$capability', False)
    print('true' if cap_value else 'false')
except:
    print('false')
" < <(get_session_state_v2 "$session_id" 2>/dev/null || echo "{}"))
    
    [[ "$has_cap" == "true" ]]
}

# Get all capabilities for a session
get_session_capabilities() {
    local session_id="$1"
    
    python3 -c "
import json
import sys

try:
    state = json.loads(sys.stdin.read())
    capabilities = state.get('coordination', {}).get('capabilities', {})
    for cap, enabled in capabilities.items():
        if enabled:
            print(cap.replace('can_', ''))
except:
    pass
" < <(get_session_state_v2 "$session_id" 2>/dev/null || echo "{}")
}

# Set capabilities based on role
set_capabilities_for_role() {
    local session_id="$1"
    local role="$2"
    
    # Get capabilities for role
    local caps="${ROLE_CAPABILITIES[$role]:-}"
    if [[ -z "$caps" ]]; then
        echo "Unknown role: $role" >&2
        return 1
    fi
    
    # Set capabilities
    IFS=',' read -ra cap_array <<< "$caps"
    set_session_capabilities "$session_id" "${cap_array[@]}"
    
    # Update preferred role using Python
    python3 -c "
import json
from datetime import datetime

state_file = '${FLOWLOOM_DIR}/sessions/${session_id}/state.json'

with open(state_file, 'r') as f:
    state = json.load(f)

state['coordination']['preferred_role'] = '$role'
state['lastActivity'] = datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ')

with open(state_file, 'w') as f:
    json.dump(state, f, indent=2)
"
    
    # Update registry
    update_registry_indices "$session_id"
}

# Find best session for task
find_best_session_for_task() {
    local task_type="$1"
    shift
    local required_capabilities=("$@")
    
    # Convert bash array to Python list
    local caps_str=""
    for cap in "${required_capabilities[@]}"; do
        caps_str="${caps_str}'can_${cap}',"
    done
    caps_str="[${caps_str%,}]"
    
    local best_session=$(python3 -c "
import json
import os

flowloom_dir = '${FLOWLOOM_DIR}'
registry_file = os.path.join(flowloom_dir, 'coordination', 'registry.json')
required_caps = $caps_str

best_session = ''
best_score = 0
candidates = {}

# Load registry
try:
    with open(registry_file, 'r') as f:
        registry = json.load(f)
except:
    exit(0)

# Score each active session
for session_id, info in registry.get('sessions', {}).items():
    if info.get('status') != 'active':
        continue
    
    # Load session state
    state_file = os.path.join(info['endpoint'], 'state.json')
    try:
        with open(state_file, 'r') as f:
            state = json.load(f)
    except:
        continue
    
    # Calculate capability score
    score = 0
    has_all = True
    capabilities = state.get('coordination', {}).get('capabilities', {})
    
    for cap in required_caps:
        if capabilities.get(cap, False):
            score += 1
        else:
            has_all = False
    
    # Prefer sessions with all required capabilities
    if has_all:
        score += 100
    
    # Check current workload
    queue_size = len(state.get('tasks', {}).get('queue', []))
    score -= queue_size * 10
    
    candidates[session_id] = score
    
    if score > best_score:
        best_score = score
        best_session = session_id

if best_session:
    print(best_session)
    # Log candidates for debugging
    import sys
    print(f'Candidates: {candidates}', file=sys.stderr)
")
    
    if [[ -n "$best_session" ]]; then
        echo "$best_session"
        log_coordination_event "task_assignment" "$best_session" \
            "Selected for task '$task_type' (required: ${required_capabilities[*]})" "INFO"
    else
        log_coordination_event "task_assignment_failed" "system" \
            "No suitable session for task '$task_type' requiring: ${required_capabilities[*]}" "WARN"
        return 1
    fi
}

# Calculate capability match score
calculate_capability_score() {
    local session_id="$1"
    shift
    local required_capabilities=("$@")
    
    local score=0
    for cap in "${required_capabilities[@]}"; do
        if session_has_capability "$session_id" "can_$cap"; then
            ((score++))
        fi
    done
    
    echo "$score"
}

# Get capability statistics
get_capability_stats() {
    python3 -c "
import json
import os

flowloom_dir = '${FLOWLOOM_DIR}'
registry_file = os.path.join(flowloom_dir, 'coordination', 'registry.json')

stats = {}
capability_names = [
    'can_implement', 'can_review', 'can_test', 
    'can_document', 'can_coordinate', 'can_analyze'
]

# Initialize stats
for cap in capability_names:
    stats[cap] = 0

# Load registry and count
try:
    with open(registry_file, 'r') as f:
        registry = json.load(f)
    
    for cap in capability_names:
        sessions = registry.get('index', {}).get('by_capability', {}).get(cap, [])
        stats[cap] = len(sessions)
except:
    pass

print(json.dumps(stats, indent=2))
"
}

# Recommend capabilities to balance workload
recommend_capability_distribution() {
    python3 -c "
import json
import os

flowloom_dir = '${FLOWLOOM_DIR}'
registry_file = os.path.join(flowloom_dir, 'coordination', 'registry.json')

recommendations = []

# Get stats
stats = {}
capability_names = [
    'can_implement', 'can_review', 'can_test', 
    'can_document', 'can_coordinate', 'can_analyze'
]

try:
    with open(registry_file, 'r') as f:
        registry = json.load(f)
    
    for cap in capability_names:
        sessions = registry.get('index', {}).get('by_capability', {}).get(cap, [])
        stats[cap] = len(sessions)
except:
    stats = {cap: 0 for cap in capability_names}

# Check for missing capabilities
for cap, count in stats.items():
    if count == 0:
        recommendations.append(f'CRITICAL: No sessions with {cap} capability')
    elif count == 1:
        recommendations.append(f'WARNING: Only one session with {cap} capability')

# Check balance
implementers = stats.get('can_implement', 0)
reviewers = stats.get('can_review', 0)

if implementers > 0 and reviewers == 0:
    recommendations.append('WARNING: Implementers without reviewers')

if not recommendations:
    recommendations.append('INFO: Capability distribution is balanced')

for rec in recommendations:
    print(rec)
"
}

# Export functions
export -f session_has_capability
export -f get_session_capabilities
export -f set_capabilities_for_role
export -f find_best_session_for_task
export -f calculate_capability_score
export -f get_capability_stats
export -f recommend_capability_distribution