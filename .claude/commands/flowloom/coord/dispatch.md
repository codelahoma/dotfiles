Dispatch tasks to available workers with capability matching

Assign a task to an available worker using permission-free MCP operations with proper Shell_ID tagging.

## Usage:
```
/coord:dispatch [task_description] [required_capabilities] [priority]
```

Where:
- task_description: Description of the task to be performed
- required_capabilities: Required worker capabilities (optional, default: any)
- priority: Task priority (high/medium/low, default: medium)

## Permission-Free Implementation
Uses direct MCP memory operations and cached session ID, following FlowLoom's Permission Prompt Avoidance policy.

Parse arguments and set defaults:

@bash
task_description="$1"
required_capabilities="${2:-any}"
priority="${3:-medium}"

if [ -z "$task_description" ]; then
  echo "❌ Error: Task description required"
  echo "Usage: /coord:dispatch [task_description] [required_capabilities] [priority]"
  echo ""
  echo "Example: /coord:dispatch 'Implement user authentication' coding,security high"
  exit 1
fi

# Use cached session ID and create task ID
session_id="85299"  # Cached active session
timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
task_id="task_$(date +%s)_${session_id}"

echo "📋 Dispatching Task (Permission-Free)"
echo "====================================="
echo ""
echo "📝 Task Details:"
echo "   Session: $session_id"
echo "   Task ID: $task_id"
echo "   Description: $task_description"
echo "   Required Capabilities: $required_capabilities"
echo "   Priority: $priority"
echo "   Created: $timestamp"
echo ""
@

Find available workers and assign task using permission-free operations:

@memory
# Determine worker assignment based on capabilities
echo "🔍 Finding Available Workers:"
if [ "$required_capabilities" = "any" ]; then
  echo "   Looking for any available worker..."
  # Simple assignment logic for current session
  if [ -z "$(echo 'worker_91625 worker_4197' | grep worker_91625)" ]; then
    assigned_worker="worker_91625"
    echo "   ✅ Assigned to worker_91625 (research specialist)"
  else
    assigned_worker="worker_4197" 
    echo "   ✅ Assigned to worker_4197 (coding specialist)"
  fi
else
  echo "   Looking for worker with capabilities: $required_capabilities"
  # Match capabilities to workers
  case "$required_capabilities" in
    *research*|*analysis*|*documentation*)
      assigned_worker="worker_91625"
      echo "   ✅ Matched worker_91625 (research, analysis, documentation)"
      ;;
    *coding*|*testing*|*implementation*)
      assigned_worker="worker_4197"
      echo "   ✅ Matched worker_4197 (coding, testing, implementation)"
      ;;
    *)
      assigned_worker="worker_91625"  # Default to research worker
      echo "   ✅ Default assignment to worker_91625"
      ;;
  esac
fi
echo ""
@

Create task entity with proper Shell_ID tagging:

@memory
# Create task entity with comprehensive tagging
mcp__memory__create_entities '{
  "entities": [
    {
      "name": "Task '${task_id}'",
      "entityType": "CoordinationTask",
      "observations": [
        "Shell_ID: 85299 - '${timestamp}' | Task created via permission-free dispatch",
        "Task ID: '${task_id}'",
        "Description: '${task_description}'",
        "Status: assigned",
        "Priority: '${priority}'",
        "Required Capabilities: '${required_capabilities}'",
        "Assigned Worker: '${assigned_worker}'",
        "Session: '${session_id}'",
        "Created: '${timestamp}'",
        "Dispatcher: controller_85299",
        "Dispatch Method: Permission-free MCP operations"
      ]
    }
  ]
}'

# Update assigned worker status
mcp__memory__add_observations '{
  "observations": [
    {
      "entityName": "Claude Worker '${assigned_worker}'",
      "contents": [
        "Shell_ID: '${assigned_worker}' - '${timestamp}' | New task assigned via permission-free dispatch",
        "Status: Busy",
        "Current Task: '${task_id}'",
        "Task Assigned: '${timestamp}'"
      ]
    }
  ]
}'
@

Display dispatch success and next steps:

@bash
echo "✅ Task Dispatch Complete"
echo ""
echo "📊 Assignment Summary:"
echo "   Task ID: $task_id"
echo "   Assigned Worker: $assigned_worker"
echo "   Status: Assigned and active"
echo "   Priority: $priority"
echo ""
echo "💡 Next Actions:"
echo "   /coord:status - Monitor task progress"
echo "   /coord:cancel $task_id - Cancel if needed"
echo "   /worker:complete - Worker completion command"
echo ""
echo "📬 Task successfully dispatched via permission-free operations!"
@

Create and assign tasks using permission-free MCP operations with enhanced worker matching, proper Shell_ID tagging, and comprehensive status tracking.