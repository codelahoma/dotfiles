#!/bin/bash
# Test Coordination Infrastructure
set -euo pipefail

# Test environment setup
export FLOWLOOM_DIR="${FLOWLOOM_TEST_DIR:-/tmp/flowloom-test-$$}"
mkdir -p "$FLOWLOOM_DIR/bin"

# Copy scripts to test environment
cp bin/session_state_v2.sh "$FLOWLOOM_DIR/bin/"
cp bin/session_registry.sh "$FLOWLOOM_DIR/bin/"
cp bin/coordination_locking.sh "$FLOWLOOM_DIR/bin/"
cp bin/coordination_audit.sh "$FLOWLOOM_DIR/bin/"
cp bin/coordination_init.sh "$FLOWLOOM_DIR/bin/"

# Source modules
source "$FLOWLOOM_DIR/bin/session_state_v2.sh"
source "$FLOWLOOM_DIR/bin/session_registry.sh"
source "$FLOWLOOM_DIR/bin/coordination_locking.sh"
source "$FLOWLOOM_DIR/bin/coordination_audit.sh"

# Test counters
TESTS_RUN=0
TESTS_PASSED=0

# Test helper
run_test() {
    local test_name="$1"
    local test_function="$2"
    
    echo -n "Testing $test_name... "
    TESTS_RUN=$((TESTS_RUN + 1))
    
    if $test_function; then
        echo "PASSED"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo "FAILED"
    fi
}

# Helper function to get JSON value using Python
get_json_value() {
    local file="$1"
    local path="$2"
    
    python3 -c "
import json

try:
    with open('$file', 'r') as f:
        data = json.load(f)
    
    # Parse path like '.version' or '.coordination.preferred_role'
    keys = '$path'.strip('.').split('.')
    value = data
    for key in keys:
        if isinstance(value, dict) and key in value:
            value = value[key]
        else:
            print('null')
            exit()
    
    if isinstance(value, bool):
        print('true' if value else 'false')
    else:
        print(value)
except:
    print('null')
"
}

# Test 1: State initialization
test_state_init() {
    local session_id="test-$$-1"
    local state_file=$(init_session_state_v2 "$session_id" $$ "reviewer")
    
    [[ -f "$state_file" ]] && \
    [[ $(get_json_value "$state_file" "version") == "1" ]] && \
    [[ $(get_json_value "$state_file" "coordination.preferred_role") == "reviewer" ]]
}

# Test 2: Capability setting
test_capabilities() {
    local session_id="test-$$-2"
    init_session_state_v2 "$session_id" $$ "implementer" >/dev/null
    
    set_session_capabilities "$session_id" "implement" "test" "review"
    
    local state_file="${FLOWLOOM_DIR}/sessions/${session_id}/state.json"
    [[ $(get_json_value "$state_file" "coordination.capabilities.can_implement") == "true" ]] && \
    [[ $(get_json_value "$state_file" "coordination.capabilities.can_test") == "true" ]] && \
    [[ $(get_json_value "$state_file" "coordination.capabilities.can_review") == "true" ]]
}

# Test 3: Registry operations
test_registry() {
    init_registry
    
    local session_id="test-$$-3"
    mkdir -p "${FLOWLOOM_DIR}/sessions/${session_id}"
    init_session_state_v2 "$session_id" $$ "coordinator" >/dev/null
    set_session_capabilities "$session_id" "coordinate" "review"
    
    register_session "$session_id" "${FLOWLOOM_DIR}/sessions/${session_id}"
    
    # Test lookups
    local found_by_role=$(find_by_role "coordinator")
    local found_by_cap=$(find_by_capability "can_coordinate")
    
    [[ "$found_by_role" == "$session_id" ]] && \
    [[ "$found_by_cap" == "$session_id" ]]
}

# Test 4: State locking
test_locking() {
    local session_id="test-$$-4"
    mkdir -p "${FLOWLOOM_DIR}/sessions/${session_id}"
    init_session_state_v2 "$session_id" $$ "tester" >/dev/null
    
    # Test lock acquisition and release
    acquire_state_lock "$session_id" && \
    is_locked "$session_id" && \
    release_state_lock "$session_id" && \
    ! is_locked "$session_id"
}

# Test 5: Atomic updates
test_atomic_updates() {
    local session_id="test-$$-5"
    mkdir -p "${FLOWLOOM_DIR}/sessions/${session_id}"
    init_session_state_v2 "$session_id" $$ "implementer" >/dev/null
    
    # Perform atomic update
    with_state_lock "$session_id" update_state_atomic "set_task" "test-task" "Test task description"
    
    local state_file="${FLOWLOOM_DIR}/sessions/${session_id}/state.json"
    [[ $(get_json_value "$state_file" "tasks.current.id") == "test-task" ]]
}

# Test 6: Audit logging
test_audit() {
    init_audit_system
    
    log_coordination_event "test_event" "test-session" "Test audit event" "INFO"
    
    local audit_file="${AUDIT_LOG_DIR}/audit-$(date +%Y%m%d).log"
    [[ -f "$audit_file" ]] && \
    grep -q "test_event" "$audit_file"
}

# Run all tests
echo "=== Coordination Infrastructure Tests ==="
run_test "State initialization" test_state_init
run_test "Capability management" test_capabilities
run_test "Registry operations" test_registry
run_test "State locking" test_locking
run_test "Atomic updates" test_atomic_updates
run_test "Audit logging" test_audit

echo
echo "Tests completed: $TESTS_PASSED/$TESTS_RUN passed"

# Cleanup
rm -rf "$FLOWLOOM_DIR"

[[ $TESTS_PASSED -eq $TESTS_RUN ]]