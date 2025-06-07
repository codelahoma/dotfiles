# Manage Coordination Workflow

Start, monitor, or control multi-step workflows across coordination sessions.

## Usage:
```
/coord:workflow [action] [workflow_name] [additional_args...]
```

Actions:
- `start [workflow_name] [description]` - Start a new workflow
- `status [workflow_name]` - Check workflow progress  
- `pause [workflow_name]` - Pause workflow execution
- `resume [workflow_name]` - Resume paused workflow
- `complete [workflow_name]` - Mark workflow as completed

@bash
action="$1"
workflow_name="$2"
shift 2 2>/dev/null || true
additional_args="$*"

if [ -z "$action" ]; then
  echo "❌ Error: Action required"
  echo "Usage: /coord:workflow [action] [workflow_name] [additional_args...]"
  echo ""
  echo "Actions:"
  echo "  start [workflow_name] [description] - Start new workflow"
  echo "  status [workflow_name] - Check progress"
  echo "  pause [workflow_name] - Pause execution"
  echo "  resume [workflow_name] - Resume execution"
  echo "  complete [workflow_name] - Mark completed"
  exit 1
fi

shell_pid=$(./bin/get_shell_pid.sh)

case "$action" in
  "start")
    if [ -z "$workflow_name" ]; then
      echo "❌ Error: Workflow name required for start action"
      exit 1
    fi
    
    description="${additional_args:-Multi-step coordination workflow}"
    workflow_id="workflow_${workflow_name}_${shell_pid}"
    
    echo "🚀 Starting coordination workflow"
    echo "Workflow: $workflow_name"
    echo "ID: $workflow_id"
    echo "Description: $description"
    echo ""
    
    # Create workflow entity
    mcp__memory__create_entities '{
      "entities": [
        {
          "name": "Coordination Workflow '${workflow_id}'",
          "entityType": "CoordinationWorkflow",
          "observations": [
            "Workflow ID: '${workflow_id}'",
            "Name: '${workflow_name}'",
            "Description: '${description}'",
            "Status: active",
            "Phase: initialization",
            "Session: '${shell_pid}'",
            "Controller: controller_'${shell_pid}'",
            "Started: $(date -u +"%Y-%m-%dT%H:%M:%SZ")",
            "Progress: 0%"
          ]
        }
      ]
    }'
    
    # Link to coordination session
    mcp__memory__create_relations '{
      "relations": [
        {
          "from": "Coordination Session '${shell_pid}'",
          "to": "Coordination Workflow '${workflow_id}'",
          "relationType": "manages_workflow"
        }
      ]
    }'
    
    echo "✅ Workflow '$workflow_name' started successfully"
    echo "Workflow ID: $workflow_id"
    ;;
    
  "status")
    if [ -z "$workflow_name" ]; then
      echo "🔍 All workflows in session $shell_pid:"
      mcp__memory__search_nodes '{
        "query": "Session: '${shell_pid}' CoordinationWorkflow"
      }' | grep -A8 "CoordinationWorkflow" | grep -E "(name|Status|Phase|Progress)" | sed 's/^/   /'
    else
      workflow_id="workflow_${workflow_name}_${shell_pid}"
      echo "🔍 Workflow Status: $workflow_name"
      echo "================================"
      mcp__memory__search_nodes '{
        "query": "Coordination Workflow '${workflow_id}'"
      }' | grep -E "(Status|Phase|Progress|Started)" | sed 's/^/   /'
      
      # Show related tasks
      echo ""
      echo "Related Tasks:"
      mcp__memory__search_nodes '{
        "query": "workflow: '${workflow_name}'"
      }' | grep -A3 -B1 "Task" | grep -E "(name|Status|Assigned)" | sed 's/^/   /'
    fi
    ;;
    
  "pause")
    if [ -z "$workflow_name" ]; then
      echo "❌ Error: Workflow name required for pause action"
      exit 1
    fi
    
    workflow_id="workflow_${workflow_name}_${shell_pid}"
    echo "⏸️  Pausing workflow: $workflow_name"
    
    mcp__memory__add_observations '{
      "observations": [
        {
          "entityName": "Coordination Workflow '${workflow_id}'",
          "contents": [
            "Status: paused",
            "Paused: $(date -u +"%Y-%m-%dT%H:%M:%SZ")",
            "Paused By: controller_'${shell_pid}'"
          ]
        }
      ]
    }'
    
    echo "✅ Workflow '$workflow_name' paused"
    ;;
    
  "resume")
    if [ -z "$workflow_name" ]; then
      echo "❌ Error: Workflow name required for resume action"
      exit 1
    fi
    
    workflow_id="workflow_${workflow_name}_${shell_pid}"
    echo "▶️  Resuming workflow: $workflow_name"
    
    mcp__memory__add_observations '{
      "observations": [
        {
          "entityName": "Coordination Workflow '${workflow_id}'",
          "contents": [
            "Status: active",
            "Resumed: $(date -u +"%Y-%m-%dT%H:%M:%SZ")",
            "Resumed By: controller_'${shell_pid}'"
          ]
        }
      ]
    }'
    
    echo "✅ Workflow '$workflow_name' resumed"
    ;;
    
  "complete")
    if [ -z "$workflow_name" ]; then
      echo "❌ Error: Workflow name required for complete action"
      exit 1
    fi
    
    workflow_id="workflow_${workflow_name}_${shell_pid}"
    echo "🏁 Completing workflow: $workflow_name"
    
    mcp__memory__add_observations '{
      "observations": [
        {
          "entityName": "Coordination Workflow '${workflow_id}'",
          "contents": [
            "Status: completed",
            "Phase: finalization",
            "Progress: 100%",
            "Completed: $(date -u +"%Y-%m-%dT%H:%M:%SZ")",
            "Completed By: controller_'${shell_pid}'"
          ]
        }
      ]
    }'
    
    echo "✅ Workflow '$workflow_name' completed successfully"
    ;;
    
  *)
    echo "❌ Unknown action: $action"
    echo "Valid actions: start, status, pause, resume, complete"
    exit 1
    ;;
esac
@

Manage complex multi-step workflows within the coordination session, tracking workflow state and progress through the memory graph for comprehensive orchestration.