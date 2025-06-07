# Memory Query Command

Execute SQL-like queries against FlowLoom's memory graph for analysis and coordination.

## Quick Usage

```bash
/flowloom:memory:query "SELECT * FROM entities WHERE entityType = 'Task'"
```

## Implementation

```bash
# Execute query with table output (default)
./bin/memory-monitor query memory.json "$ARGUMENTS" --output table

# For JSON output (useful for scripting)
./bin/memory-monitor query memory.json "$ARGUMENTS" --output json

# For CSV output (useful for analysis)
./bin/memory-monitor query memory.json "$ARGUMENTS" --output csv
```

## Common Query Patterns

### Find Active Tasks
```sql
SELECT * FROM entities 
WHERE entityType = 'Task' 
  AND observations CONTAINS 'status:active'
```

### Search by Content
```sql
SELECT * FROM entities 
WHERE observations CONTAINS 'authentication'
  OR observations CONTAINS 'login'
```

### Recent Activity
```sql
SELECT * FROM entities 
WHERE created > '2025-05-28T00:00:00Z'
ORDER BY created DESC
```

### High Priority Items
```sql
SELECT * FROM entities 
WHERE observations MATCH 'priority:\s*high'
  OR observations CONTAINS 'urgent'
```

### Multi-Claude Coordination
```sql
-- Find assigned tasks
SELECT * FROM entities 
WHERE entityType = 'CoordinationTask'
  AND observations CONTAINS 'assigned_to:Worker-'

-- Check completion status  
SELECT * FROM entities
WHERE observations CONTAINS 'status:completed'
  AND created > '2025-05-28'
```

## Advanced Features

### Pagination
```sql
SELECT * FROM entities 
WHERE entityType = 'Task'
ORDER BY created DESC
LIMIT 10 OFFSET 20
```

### Complex Conditions
```sql
SELECT * FROM entities
WHERE entityType IN ('Task', 'WorkItem', 'Bug')
  AND (
    observations CONTAINS 'priority:high' 
    OR observations CONTAINS 'urgent'
  )
  AND NOT observations CONTAINS 'status:completed'
```

### Pattern Matching
```sql
-- Regex pattern matching
SELECT * FROM entities 
WHERE observations MATCH 'status:\s*(active|pending|assigned)'

-- Partial string matching
SELECT * FROM entities
WHERE name LIKE '%authentication%'
```

## Output Formats

### Table Format
Best for human reading:
```
Results: 3 total
Query: SELECT * FROM entities WHERE entityType = 'Task'
--------------------------------------------------------------------------------
1. User Authentication Task (Task)
   • Implement OAuth2 authentication system
   • Add JWT token management
   • Create login/logout endpoints

2. Database Migration Task (Task)
   • Migrate user tables to new schema
   • Update foreign key relationships
   
Execution: 1.23ms, scanned 156 entities
```

### JSON Format
Best for scripting and integration:
```json
{
  "query": "SELECT * FROM entities WHERE entityType = 'Task'",
  "results": [
    {
      "name": "User Authentication Task",
      "entityType": "Task",
      "observations": ["Implement OAuth2 authentication system", ...]
    }
  ],
  "count": 3,
  "stats": {
    "execution_time_ms": 1.23,
    "entities_scanned": 156
  }
}
```

### CSV Format
Best for spreadsheet analysis:
```
name,entityType,observations
"User Authentication Task","Task","Implement OAuth2;Add JWT tokens"
"Database Migration Task","Task","Migrate user tables;Update relationships"
```

## Performance Tips

1. **Use Indexed Fields**: Queries on `entityType` and `name` are fastest
2. **Limit Results**: Use LIMIT for large datasets
3. **Specific Conditions**: More specific WHERE clauses run faster
4. **Avoid Complex Regex**: Simple CONTAINS is faster than MATCH

## Integration Examples

### With Coordination Commands
```bash
# Find my assigned tasks
task_count=$(./bin/memory-monitor query memory.json \
  "SELECT * FROM entities WHERE observations CONTAINS 'assigned_to:$worker_id'" \
  --output json | jq '.count')

if [ "$task_count" -gt 0 ]; then
  echo "You have $task_count assigned tasks"
fi
```

### With Planning Workflows
```bash
# Check plan status
./bin/memory-monitor query memory.json \
  "SELECT * FROM entities WHERE entityType = 'Plan' AND observations CONTAINS 'status:'" \
  --output table
```

### With Session Management
```bash
# Find session entities
./bin/memory-monitor query memory.json \
  "SELECT * FROM entities WHERE observations CONTAINS 'Shell_ID:$shell_pid'" \
  --output json
```

This command provides powerful querying capabilities that transform FlowLoom's memory into a searchable, analyzable knowledge base.