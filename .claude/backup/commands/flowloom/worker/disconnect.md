# Disconnect Worker from Coordination Session

Gracefully disconnect this worker from the coordination session.

## Usage:
```
/worker:disconnect [reason]
```

Where:
- reason: Optional reason for disconnection (default: "Worker requested disconnect")

@bash
reason="${1:-Worker requested disconnect}"

shell_pid=$(./bin/get_shell_pid.sh)
worker_id="worker_${shell_pid}"

echo "👋 Disconnecting worker from coordination session"
echo "Worker ID: $worker_id"
echo "Reason: $reason"
echo ""

# Check if worker is registered
worker_info=$(mcp__memory__search_nodes '{
  "query": "Claude Worker '${worker_id}'"
}')

if ! echo "$worker_info" | grep -q "ClaudeWorker"; then
  echo "❌ Worker '$worker_id' not found in any coordination session"
  echo "Worker may already be disconnected or never registered"
  exit 1
fi

# Get current session and task assignment
session_id=$(echo "$worker_info" | grep "Session:" | sed 's/.*Session: //' | tr -d '"')
current_task=$(echo "$worker_info" | grep "Current Task:" | sed 's/.*Current Task: //' | tr -d '"')

# Check if worker has active task
if [ "$current_task" != "none" ] && [ -n "$current_task" ]; then
  echo "⚠️  Worker has active task: $current_task"
  echo ""
  read -p "Cancel current task and disconnect? (y/N): " confirm
  
  if [[ "$confirm" != "y" && "$confirm" != "Y" ]]; then
    echo "❌ Disconnect cancelled - complete current task first or use /worker:complete"
    exit 0
  fi
  
  # Cancel the current task
  echo "🚫 Cancelling current task: $current_task"
  mcp__memory__add_observations '{
    "observations": [
      {
        "entityName": "Task '${current_task}'",
        "contents": [
          "Status: cancelled",
          "Cancelled: $(date -u +"%Y-%m-%dT%H:%M:%SZ")",
          "Cancelled By: '${worker_id}'",
          "Reason: Worker disconnect"
        ]
      }
    ]
  }'
fi

# Disconnect the worker
mcp__memory__add_observations '{
  "observations": [
    {
      "entityName": "Claude Worker '${worker_id}'",
      "contents": [
        "Status: Disconnected",
        "Session: none",
        "Current Task: none",
        "Disconnected: $(date -u +"%Y-%m-%dT%H:%M:%SZ")",
        "Disconnect Reason: '${reason}'"
      ]
    }
  ]
}'

echo "✅ Worker '$worker_id' disconnected successfully"
if [ -n "$session_id" ]; then
  echo "   - Removed from session: $session_id"
fi
if [ "$current_task" != "none" ] && [ -n "$current_task" ]; then
  echo "   - Cancelled task: $current_task"
fi

echo ""
echo "💡 Available Actions:"
echo "   /worker:announce [session_id] - Join a different coordination session"
echo "   /worker:status - View current worker state"
@

Gracefully disconnect worker from coordination session with optional task cancellation and proper cleanup of worker state.