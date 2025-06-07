# Set Coordination Mode

Set the coordination mode for this Claude instance based on role and session.

## Usage:
```
/coord:mode [role] [session_id]
```

Where:
- role: worker, controller, conductor (default: worker)
- session_id: The coordination session ID (default: 85299)

@bash
role="${1:-worker}"
session_id="${2:-85299}"
shell_pid=$(./bin/get_shell_pid.sh)

case "$role" in
  "worker")
    mode_name="Worker Mode (Session $session_id)"
    ;;
  "controller"|"conductor")
    mode_name="Controller Mode (Session $session_id)"
    ;;
  *)
    echo "❌ Invalid role: $role"
    echo "Valid roles: worker, controller, conductor"
    exit 1
    ;;
esac

echo "🎭 Setting Coordination Mode"
echo "============================="
echo ""
echo "Role: $role"
echo "Session: $session_id"
echo "Mode: $mode_name"
echo "Shell PID: $shell_pid"
echo ""

# Update worker/controller entity with mode information
if [ "$role" = "worker" ]; then
  entity_name="Claude Worker worker_${shell_pid}"
  entity_type="ClaudeWorker"
else
  entity_name="Coordination Session ${session_id}"
  entity_type="CoordinationSession"
fi

# Add mode observation to entity
mcp__memory__add_observations '{
  "observations": [
    {
      "entityName": "'${entity_name}'",
      "contents": [
        "Active Mode: '${mode_name}'",
        "Mode Set: $(date -u +\"%Y-%m-%dT%H:%M:%SZ\")",
        "Shell_ID: '${shell_pid}' - $(date -u +\"%Y-%m-%dT%H:%M:%SZ\") | Coordination mode activated: '${mode_name}'"
      ]
    }
  ]
}'

echo "✅ Coordination mode set: $mode_name"
echo ""
echo "Use this mode for:"
if [ "$role" = "worker" ]; then
  echo "  - Receiving task assignments"
  echo "  - Executing work items"
  echo "  - Reporting progress and results"
  echo "  - Collaborating in coordination workflows"
else
  echo "  - Managing coordination sessions"
  echo "  - Dispatching tasks to workers"
  echo "  - Monitoring system status"
  echo "  - Orchestrating multi-Claude workflows"
fi
@

Set the coordination mode to indicate this Claude's role in the multi-Claude coordination system.