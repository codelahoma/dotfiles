# Announce Worker to Coordination Session

Register this Claude instance as a worker using permission-free MCP operations with proper Shell_ID tagging.

## Usage:
```
/worker:announce [session_id] [capabilities]
# Shorthand: /worker:announce
# Full: slashload worker/announce
```

Where:
- session_id: The coordination session to join (default: 85299)
- capabilities: Comma-separated list of worker capabilities

## Permission-Free Implementation
Uses direct MCP memory operations and cached shell PID, following FlowLoom's Permission Prompt Avoidance policy.

Parse arguments and set defaults:

@bash
session_id="${1:-85299}"  # Default to current active session
capabilities="${2:-research,analysis,documentation}"
timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

echo "🤝 Announcing Worker to Coordination Session (Permission-Free)"
echo "=============================================================="
echo ""
echo "📋 Registration Details:"
echo "   Target Session: $session_id"
echo "   Capabilities: $capabilities"
echo "   Registration Time: $timestamp"
echo ""
@

Get worker shell PID and create worker ID:

@memory
# Get shell PID once and cache it
shell_pid=$(./bin/get_shell_pid.sh)
worker_id="worker_${shell_pid}"

echo "🆔 Worker Identity:"
echo "   Worker ID: $worker_id"
echo "   Shell PID: $shell_pid (cached for session)"
echo ""
@

Register worker with proper Shell_ID tagging:

@memory
# Create worker entity with proper Shell_ID tagging
mcp__memory__create_entities '{
  "entities": [
    {
      "name": "Claude Worker '${worker_id}'",
      "entityType": "ClaudeWorker",
      "observations": [
        "Shell_ID: '${shell_pid}' - '${timestamp}' | Worker announced to coordination session",
        "Worker ID: '${worker_id}'",
        "Role: Worker",
        "Status: Available", 
        "Session: '${session_id}'",
        "Capabilities: ['$(echo "$capabilities" | sed "s/,/', '/g")']",
        "Last Heartbeat: '${timestamp}'",
        "Shell PID: '${shell_pid}'",
        "Announcement Method: Permission-free MCP operations"
      ]
    }
  ]
}'

# Create relationship to coordination session
mcp__memory__create_relations '{
  "relations": [
    {
      "from": "Coordination Session '${session_id}'",
      "to": "Claude Worker '${worker_id}'",
      "relationType": "has_worker"
    }
  ]
}'

# Set worker coordination mode
mcp__memory__add_observations '{
  "observations": [
    {
      "entityName": "Claude Worker '${worker_id}'",
      "contents": [
        "Active Mode: Worker Mode (Session ${session_id})",
        "Mode Set: '${timestamp}'",
        "Shell_ID: '${shell_pid}' - '${timestamp}' | Coordination mode activated automatically on announce"
      ]
    }
  ]
}'
@

Display registration success and next steps:

@bash
echo "✅ Worker Registration Complete"
echo ""
echo "📊 Registration Summary:"
echo "   Worker ID: $worker_id"
echo "   Joined Session: $session_id"
echo "   Capabilities: $capabilities"
echo "   Status: Available for task assignments"
echo ""
echo "🎯 Worker Capabilities:"
echo "   $(echo "$capabilities" | tr ',' '\n' | sed 's/^/✓ /')"
echo ""
echo "💡 Next Steps:"
echo "   /worker:status - View your worker state"
echo "   /coord:status - Check coordination session status"
echo "   /coord:dispatch - Controller can assign tasks"
echo ""
echo "⏳ Ready to receive task assignments from controller..."
@

Register this Claude instance as a worker using permission-free operations with enhanced visibility, proper Shell_ID tagging, and comprehensive status reporting.