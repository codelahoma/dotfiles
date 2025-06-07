# Complete Assigned Task

Mark a task as completed and provide results.

## Usage:
```
/worker:complete [task_id] [results_summary]
```

Where:
- task_id: The ID of the task to mark complete
- results_summary: Summary of work completed (optional)

@bash
task_id="$1"
results_summary="${2:-Task completed successfully}"

if [ -z "$task_id" ]; then
  echo "❌ Error: Task ID required"
  echo "Usage: /worker:complete [task_id] [results_summary]"
  echo ""
  echo "Example: /worker:complete task_1234567890_37783 'Authentication system implemented'"
  exit 1
fi

shell_pid=$(./bin/get_shell_pid.sh)
worker_id="worker_${shell_pid}"

echo "✅ Completing task"
echo "Task ID: $task_id"
echo "Worker ID: $worker_id"
echo "Results: $results_summary"
echo ""

# Verify task exists and is assigned to this worker
task_check=$(mcp__memory__search_nodes '{
  "query": "Task '${task_id}'"
}')

if ! echo "$task_check" | grep -q "Task"; then
  echo "❌ Task '$task_id' not found"
  echo "Available tasks:"
  mcp__memory__search_nodes '{
    "query": "CoordinationTask"
  }' | grep -E "name.*Task" | sed 's/^/   /'
  exit 1
fi

# Check if task is assigned to this worker
assignment_check=$(echo "$task_check" | grep "Assigned Worker:")
if ! echo "$assignment_check" | grep -q "$worker_id"; then
  echo "❌ Task '$task_id' is not assigned to worker '$worker_id'"
  echo "Task assignment: $assignment_check"
  exit 1
fi

# Update task status to completed
mcp__memory__add_observations '{
  "observations": [
    {
      "entityName": "Task '${task_id}'",
      "contents": [
        "Status: completed",
        "Completed By: '${worker_id}'",
        "Completion Time: $(date -u +"%Y-%m-%dT%H:%M:%SZ")",
        "Results: '${results_summary}'",
        "Duration: $(echo "$task_check" | grep "Created:" | sed "s/.*Created: //" | xargs -I {} bash -c "echo $(($(date +%s) - $(date -d {} +%s))) seconds")"
      ]
    }
  ]
}'

# Update worker status back to available
mcp__memory__add_observations '{
  "observations": [
    {
      "entityName": "Claude Worker '${worker_id}'",
      "contents": [
        "Status: Available",
        "Last Completed Task: '${task_id}'",
        "Task Completed: $(date -u +"%Y-%m-%dT%H:%M:%SZ")",
        "Current Task: none"
      ]
    }
  ]
}'

# Create task result entity for detailed tracking
result_id="result_$(date +%s)_${shell_pid}"
mcp__memory__create_entities '{
  "entities": [
    {
      "name": "Task Result '${result_id}'",
      "entityType": "TaskResult",
      "observations": [
        "Result ID: '${result_id}'",
        "Task ID: '${task_id}'",
        "Worker ID: '${worker_id}'",
        "Summary: '${results_summary}'",
        "Completion Time: $(date -u +"%Y-%m-%dT%H:%M:%SZ")",
        "Status: completed"
      ]
    }
  ]
}'

# Link result to task
mcp__memory__create_relations '{
  "relations": [
    {
      "from": "Task '${task_id}'",
      "to": "Task Result '${result_id}'",
      "relationType": "has_result"
    }
  ]
}'

echo "✅ Task completed successfully"
echo "Result ID: $result_id"
echo "Worker '$worker_id' is now available for new tasks"
echo ""
echo "Next steps:"
echo "- Controller can use /coord:status to see updated task status"
echo "- Use /worker:status to see your current availability"
@

Mark the assigned task as completed, update worker availability, and create a detailed result record in the memory graph for coordination tracking.