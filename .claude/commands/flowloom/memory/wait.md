# Memory Wait Command

Wait for specific memory conditions to be met, enabling efficient multi-Claude coordination.

## Quick Usage

```bash
/flowloom:memory:wait "SELECT * FROM entities WHERE entityType = 'Task' AND assigned_to = 'Worker-123'"
```

## Implementation

```bash
# Wait for condition with default 5-minute timeout
./bin/memory-monitor wait-for memory.json "$ARGUMENTS" --timeout 300 --output text

# Return JSON for programmatic use
./bin/memory-monitor wait-for memory.json "$ARGUMENTS" --timeout 300 --output json
```

## Multi-Claude Coordination Patterns

### Worker Task Assignment
```sql
-- Worker waits for task assignment
SELECT * FROM entities 
WHERE entityType = 'CoordinationTask' 
  AND observations CONTAINS 'assigned_to:Worker-${worker_id}'
  AND observations CONTAINS 'status:assigned'
```

### Task Completion Monitoring
```sql
-- Conductor waits for task completion
SELECT * FROM entities
WHERE observations CONTAINS 'task_id:${task_id}'
  AND observations CONTAINS 'status:completed'
```

### Session Coordination
```sql
-- Wait for session initialization
SELECT * FROM entities
WHERE entityType = 'CoordinationSession'
  AND observations CONTAINS 'session_id:${session_id}'
  AND observations CONTAINS 'status:ready'
```

### Resource Availability
```sql
-- Wait for resource to become available
SELECT * FROM entities
WHERE entityType = 'Resource'
  AND name = '${resource_name}'
  AND observations CONTAINS 'status:available'
```

## Workflow Integration Examples

### Worker Registration Pattern
```bash
# Worker registers and waits for work
worker_id="Worker-$(date +%s)"

# Register worker
./bin/memory-monitor query memory.json \
  "INSERT INTO entities (name, entityType, observations) VALUES ('$worker_id', 'Worker', ['status:ready', 'capabilities:$capabilities'])"

# Wait for task assignment
echo "Worker $worker_id waiting for tasks..."
task=$(./bin/memory-monitor wait-for memory.json \
  "SELECT * FROM entities 
   WHERE entityType = 'CoordinationTask' 
     AND observations CONTAINS 'assigned_to:$worker_id'
     AND observations CONTAINS 'status:assigned'" \
  --timeout 600 --output json)

if [ $? -eq 0 ]; then
  echo "Task assigned: $(echo "$task" | jq -r '.results[0].name')"
else
  echo "No task assigned within timeout"
fi
```

### Conductor Task Distribution
```bash
# Conductor creates task and waits for completion
task_id="Task-$(uuidgen)"

# Create coordination task
echo "Creating task: $task_id"
# (Task creation would happen via memory entity creation)

# Wait for task completion
echo "Waiting for task completion..."
result=$(./bin/memory-monitor wait-for memory.json \
  "SELECT * FROM entities 
   WHERE observations CONTAINS 'task_id:$task_id'
     AND observations CONTAINS 'status:completed'" \
  --timeout 1800 --output json)

if [ $? -eq 0 ]; then
  echo "Task completed successfully"
  echo "$result" | jq '.results[0].observations'
else
  echo "Task did not complete within timeout"
fi
```

### Pipeline Coordination
```bash
# Wait for previous stage completion
stage1_complete=$(./bin/memory-monitor wait-for memory.json \
  "SELECT * FROM entities 
   WHERE entityType = 'PipelineStage'
     AND observations CONTAINS 'stage:design'
     AND observations CONTAINS 'status:completed'" \
  --timeout 900)

if [ $? -eq 0 ]; then
  echo "Design stage complete, starting implementation..."
  # Trigger next stage
fi
```

### Context Limit Management
```bash
# Worker with context limit awareness
while true; do
  # Wait for task with timeout
  task=$(./bin/memory-monitor wait-for memory.json \
    "SELECT * FROM entities WHERE assigned_to = '$worker_id'" \
    --timeout 300 --output json)
  
  if [ $? -eq 0 ]; then
    # Process task
    process_task "$task"
  else
    # Timeout reached - restart to clear context
    echo "Context limit timeout - restarting worker"
    restart_worker
    break
  fi
done
```

## Advanced Patterns

### Conditional Waiting
```bash
# Wait for either completion or error
result=$(./bin/memory-monitor wait-for memory.json \
  "SELECT * FROM entities 
   WHERE observations CONTAINS 'task_id:$task_id'
     AND (
       observations CONTAINS 'status:completed' 
       OR observations CONTAINS 'status:error'
     )" \
  --timeout 1200)
```

### Multi-Condition Waiting
```bash
# Wait for multiple workers to be ready
./bin/memory-monitor wait-for memory.json \
  "SELECT * FROM entities 
   WHERE entityType = 'CoordinationSession'
     AND observations CONTAINS 'workers_ready:3'" \
  --timeout 600
```

### Threshold-Based Waiting
```bash
# Wait for sufficient progress
./bin/memory-monitor wait-for memory.json \
  "SELECT * FROM entities 
   WHERE entityType = 'Progress'
     AND observations MATCH 'completion:\s*([89][0-9]|100)'" \
  --timeout 1800
```

## Error Handling

### Timeout Management
```bash
# Handle timeouts gracefully
if ! ./bin/memory-monitor wait-for memory.json "$query" --timeout 300; then
  echo "Condition not met within timeout"
  # Fallback action
  handle_timeout
fi
```

### Retry Logic
```bash
# Retry with exponential backoff
attempt=1
max_attempts=3
timeout=60

while [ $attempt -le $max_attempts ]; do
  if ./bin/memory-monitor wait-for memory.json "$query" --timeout $timeout; then
    break
  fi
  
  echo "Attempt $attempt failed, retrying..."
  timeout=$((timeout * 2))  # Exponential backoff
  attempt=$((attempt + 1))
done
```

## Performance Considerations

1. **Specific Queries**: More specific conditions reduce monitoring overhead
2. **Reasonable Timeouts**: Balance responsiveness with resource usage
3. **Index Usage**: Queries on `entityType` and `name` are most efficient
4. **Polling Interval**: Default 1-second polling balances responsiveness and load

This command enables sophisticated coordination patterns that make multi-Claude development teams possible, transforming FlowLoom from single-agent to multi-agent orchestration platform.