#!/bin/bash
# Coordination Audit - Secure audit logging
set -euo pipefail

# Audit log location
AUDIT_LOG_DIR="${FLOWLOOM_DIR:-$HOME/.flowloom}/audit"

# Initialize audit system
init_audit_system() {
    mkdir -p "$AUDIT_LOG_DIR"
    chmod 700 "$AUDIT_LOG_DIR"
    
    # Create daily audit file
    local audit_file="${AUDIT_LOG_DIR}/audit-$(date +%Y%m%d).log"
    touch "$audit_file"
    chmod 600 "$audit_file"
}

# Log coordination event
log_coordination_event() {
    local event_type="$1"
    local session_id="$2"
    local details="$3"
    local severity="${4:-INFO}"
    
    init_audit_system
    
    local audit_file="${AUDIT_LOG_DIR}/audit-$(date +%Y%m%d).log"
    local event_id=$(python3 -c "import uuid; print(str(uuid.uuid4()))")
    
    # Create audit entry using Python
    python3 -c "
import json
from datetime import datetime
import os

audit_entry = {
    'event_id': '$event_id',
    'timestamp': datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ'),
    'event_type': '$event_type',
    'session_id': '$session_id',
    'details': '$details',
    'severity': '$severity',
    'process_id': os.getpid()
}

with open('$audit_file', 'a') as f:
    f.write(json.dumps(audit_entry) + '\n')
"
    
    # Also add to session state if applicable
    if [[ -n "$session_id" ]] && [[ "$session_id" != "system" ]]; then
        add_session_audit_event "$session_id" "$event_type" "$details"
    fi
}

# Add audit event to session state
add_session_audit_event() {
    local session_id="$1"
    local event_type="$2"
    local details="$3"
    
    local state_file="${FLOWLOOM_DIR}/sessions/${session_id}/state.json"
    
    if [[ -f "$state_file" ]]; then
        source "${FLOWLOOM_DIR}/bin/coordination_locking.sh"
        
        with_state_lock "$session_id" update_state_atomic "add_audit_event" "$event_type" "$details"
    fi
}

# Query audit logs
query_audit_logs() {
    local query_type="$1"
    local query_value="$2"
    local days_back="${3:-7}"
    
    # Search through recent audit files using Python
    python3 -c "
import json
import os
from datetime import datetime, timedelta

audit_dir = '$AUDIT_LOG_DIR'
query_type = '$query_type'
query_value = '$query_value'
days_back = int('$days_back')

results = []

for i in range(days_back):
    date = (datetime.now() - timedelta(days=i)).strftime('%Y%m%d')
    audit_file = os.path.join(audit_dir, f'audit-{date}.log')
    
    if os.path.exists(audit_file):
        with open(audit_file, 'r') as f:
            for line in f:
                try:
                    entry = json.loads(line.strip())
                    
                    if query_type == 'session' and entry.get('session_id') == query_value:
                        results.append(entry)
                    elif query_type == 'event_type' and entry.get('event_type') == query_value:
                        results.append(entry)
                    elif query_type == 'severity' and entry.get('severity') == query_value:
                        results.append(entry)
                    elif query_type == 'time_range':
                        start_time, end_time = query_value.split(',')
                        if start_time <= entry.get('timestamp', '') <= end_time:
                            results.append(entry)
                except:
                    continue

print(json.dumps(results, indent=2))
"
}

# Generate audit report
generate_audit_report() {
    local report_type="${1:-daily}"
    local date="${2:-$(date +%Y-%m-%d)}"
    
    case "$report_type" in
        "daily")
            local audit_date=$(date -d "$date" +%Y%m%d 2>/dev/null || date -j -f "%Y-%m-%d" "$date" +%Y%m%d)
            local audit_file="${AUDIT_LOG_DIR}/audit-${audit_date}.log"
            
            if [[ -f "$audit_file" ]]; then
                echo "=== Coordination Audit Report for $date ==="
                echo
                
                # Use Python to generate report
                python3 -c "
import json
from collections import defaultdict

event_counts = defaultdict(int)
session_counts = defaultdict(int)
severity_counts = defaultdict(int)

with open('$audit_file', 'r') as f:
    for line in f:
        try:
            entry = json.loads(line.strip())
            event_counts[entry.get('event_type', 'unknown')] += 1
            session_counts[entry.get('session_id', 'unknown')] += 1
            severity_counts[entry.get('severity', 'unknown')] += 1
        except:
            continue

print('Event Summary:')
for event_type, count in sorted(event_counts.items()):
    print(f'  {event_type}: {count}')

print('\nSession Activity:')
for session_id, count in sorted(session_counts.items()):
    print(f'  {session_id}: {count} events')

print('\nSeverity Distribution:')
for severity, count in sorted(severity_counts.items()):
    print(f'  {severity}: {count}')
"
            else
                echo "No audit log found for $date"
            fi
            ;;
        "security")
            echo "=== Security Audit Report ==="
            echo
            echo "Failed Operations (last 7 days):"
            query_audit_logs "severity" "ERROR" 7
            echo
            echo "Lock Timeouts:"
            query_audit_logs "event_type" "lock_timeout" 7
            ;;
    esac
}

# Archive old audit logs
archive_audit_logs() {
    local days_to_keep="${1:-30}"
    local archive_dir="${AUDIT_LOG_DIR}/archive"
    
    mkdir -p "$archive_dir"
    
    # Find and compress old logs
    find "$AUDIT_LOG_DIR" -name "audit-*.log" -type f -mtime +$days_to_keep -exec gzip {} \; -exec mv {}.gz "$archive_dir/" \; 2>/dev/null || true
}

# Export functions
export -f init_audit_system
export -f log_coordination_event
export -f query_audit_logs
export -f generate_audit_report
export -f archive_audit_logs