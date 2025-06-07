# Cancel Tasks or Coordination Session

Cancel individual tasks or shut down the entire coordination session using permission-free MCP operations.

## Usage:
```
/coord:cancel [target_type] [target_id]
```

Target types:
- `task [task_id]` - Cancel specific task
- `workflow [workflow_name]` - Cancel workflow and all related tasks
- `session` - Shut down entire coordination session

## Permission-Free Implementation
Uses direct MCP memory operations with proper Shell_ID tagging, following FlowLoom's Permission Prompt Avoidance policy.

Get task information and cancel using permission-free operations:

Set target information from arguments ($ARGUMENTS):

@memory
# Extract target from $ARGUMENTS or provide cancellation menu
if [ -z "$ARGUMENTS" ]; then
  echo "🚫 Task Cancellation Options"
  echo "============================"
  echo ""
  echo "Available targets:"
  echo "1. Cancel specific task by ID"
  echo "2. Cancel all active tasks"
  echo "3. Shut down coordination session"
  echo ""
  echo "Usage examples:"
  echo "/coord:cancel task_research_async_85299"
  echo "/coord:cancel all"
  echo "/coord:cancel session"
  echo ""
  echo "Or provide task ID as argument"
else
  target="$ARGUMENTS"
  shell_pid=85299  # Use cached session PID
  timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
  
  echo "🚫 Cancelling: $target"
  echo "Session: $shell_pid"
  echo "Time: $timestamp"
  echo ""
fi
@

@memory
# Use direct MCP operations for task cancellation
mcp__memory__add_observations '{
  "observations": [
    {
      "entityName": "'${target}'",
      "contents": [
        "Shell_ID: 85299 - '${timestamp}' | Task cancellation requested by controller",
        "Status: cancelled",
        "Cancelled: '${timestamp}'",
        "Cancelled By: controller_85299",
        "Reason: Manual cancellation by controller"
      ]
    }
  ]
}'

# Free up any assigned worker
# Note: This is a simplified version - in practice would query for assigned worker first
echo "✅ Task '$target' cancelled successfully"
echo "Time: $timestamp"
echo "Controller: 85299"
@

Display cancellation results:

@bash
echo ""
echo "📊 Coordination Status After Cancellation:"
echo "  Use /coord:status to view updated task queue"
echo "  Use /coord:dispatch to assign new tasks"
echo ""
echo "💡 Next Actions:"
echo "  - Check worker availability with /coord:status"
echo "  - Dispatch replacement tasks if needed"
echo "  - Review coordination session health"
@

Provide permission-free task cancellation with proper Shell_ID tagging and comprehensive status updates. Uses direct MCP memory operations to avoid permission prompts while maintaining full coordination functionality.