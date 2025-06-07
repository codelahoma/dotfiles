# Memory Monitor Command

Monitor and query FlowLoom's memory graph with SQL-like queries and real-time streaming.

## Usage

```bash
# Query memory data
./bin/memory-monitor query memory.json "SELECT * FROM entities WHERE entityType = 'Task'"

# Watch for changes in real-time
./bin/memory-monitor watch memory.json

# Wait for specific conditions (useful for multi-Claude coordination)
./bin/memory-monitor wait-for memory.json "SELECT * FROM entities WHERE entityType = 'CoordinationTask' AND assigned_to = 'Worker-123'" --timeout 300

# Get memory statistics
./bin/memory-monitor stats memory.json

# Start WebSocket server for live UI integration
./bin/memory-monitor serve memory.json --port 8765

# Quick search functionality
./bin/memory-monitor search memory.json --entity-type Task --search "status:active"
```

## SQL Query Examples

```sql
-- Find all tasks
SELECT * FROM entities WHERE entityType = 'Task'

-- Search in observations
SELECT * FROM entities WHERE observations CONTAINS 'status:active'

-- Complex conditions with AND/OR
SELECT * FROM entities 
WHERE entityType IN ('Task', 'WorkItem') 
  AND (observations CONTAINS 'priority:high' OR observations CONTAINS 'urgent')

-- Regex pattern matching
SELECT * FROM entities WHERE observations MATCH 'status:\s*(active|pending)'

-- Sorting and pagination
SELECT * FROM entities 
WHERE entityType = 'Task' 
ORDER BY created DESC 
LIMIT 10 OFFSET 5

-- Time-based queries
SELECT * FROM entities 
WHERE created BETWEEN '2025-05-28' AND '2025-05-29'
```

## Multi-Claude Coordination

Use `wait-for` to enable efficient worker coordination:

```bash
# Worker waits for task assignment
./bin/memory-monitor wait-for memory.json \
  "SELECT * FROM entities 
   WHERE entityType = 'CoordinationTask' 
     AND observations CONTAINS 'assigned_to:Worker-${worker_id}' 
     AND observations CONTAINS 'status:assigned'" \
  --timeout 300 --output json

# Conductor monitors worker progress
./bin/memory-monitor watch memory.json | grep -E "(entity_created|entity_updated)"

# Check for completion status
./bin/memory-monitor query memory.json \
  "SELECT * FROM entities 
   WHERE entityType = 'CoordinationTask' 
     AND observations CONTAINS 'status:completed'"
```

## Output Formats

- **table**: Human-readable table format (default)
- **json**: Machine-readable JSON with metadata
- **csv**: Comma-separated values for spreadsheet import

## WebSocket Integration

Start server for real-time web UI updates:

```bash
./bin/memory-monitor serve memory.json --port 8765 --host 0.0.0.0
```

WebSocket clients receive events:
```javascript
const ws = new WebSocket('ws://localhost:8765');
ws.onmessage = (event) => {
  const data = JSON.parse(event.data);
  if (data.type === 'entity_created') {
    console.log('New entity:', data.data.entity.name);
  }
};
```

## Performance

- **Query Speed**: Sub-millisecond execution for indexed queries
- **Indexing**: Automatic indexes on `entityType` and `name` fields
- **Memory Efficient**: Streaming results for large datasets
- **Real-time**: Change detection with configurable polling intervals

## Integration with FlowLoom Commands

Memory Monitor can be integrated into FlowLoom workflows:

```bash
# In coordination commands
/conductor:dispatch "Implement user authentication"
./bin/memory-monitor wait-for memory.json "SELECT * FROM entities WHERE observations CONTAINS 'task_id:${task_id}' AND observations CONTAINS 'status:completed'"

# In worker commands  
/worker:register backend api database
./bin/memory-monitor wait-for memory.json "SELECT * FROM entities WHERE observations CONTAINS 'assigned_to:${worker_id}'"

# In monitoring workflows
./bin/memory-monitor watch memory.json | while read line; do
  echo "[$(date)] $line" >> memory-monitor.log
done
```

This transforms FlowLoom's memory from passive storage into an active, queryable intelligence system enabling sophisticated multi-Claude coordination patterns.