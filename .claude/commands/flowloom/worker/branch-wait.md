# Wait for branch-aware task assignments

Monitors for task assignments that match the worker's branch capabilities and skill set, with intelligent cross-branch coordination.

## Usage

```bash
python3 bin/branch_aware_coordinator.py worker wait --timeout 300
```

## Parameters

- `--timeout`: Maximum wait time in seconds (default: 300)

## Branch-Aware Task Matching

The system automatically matches tasks to workers based on:

1. **Branch Requirements**: Tasks specify required branches
2. **Skill Requirements**: Tasks specify needed capabilities
3. **Worker Capabilities**: Worker's branch preferences and skills
4. **Cross-Branch Rules**: Worker's cross-branch preference levels

## Wait Behavior

### Task Assignment Process
1. **Monitor**: Continuously watches for new task assignments
2. **Match**: Evaluates if assigned tasks match worker capabilities
3. **Notify**: Alerts when suitable tasks are assigned
4. **Context**: Provides branch switching guidance if needed

### Cross-Branch Assignment Handling

When assigned a task requiring a different branch:
- **Branch Switch Needed**: Shows required vs current branch
- **Capability Check**: Confirms worker can handle the branch
- **Instructions**: Provides git commands for branch switching
- **Continuity**: Maintains task assignment across branch switches

## Example Scenarios

### Same Branch Assignment
```
🎯 Branch-Aware Task Assignment Received!
=========================================
Task ID: BranchTask-abc123
Description: Update website homepage content
Priority: high
Required Branch: gh-pages
✅ Already on correct branch

💡 Next Steps:
   python3 bin/branch_aware_coordinator.py worker accept BranchTask-abc123
```

### Cross-Branch Assignment
```
🎯 Branch-Aware Task Assignment Received!
=========================================
Task ID: BranchTask-def456
Description: Run integration tests
Priority: medium
Required Branch: main
⚠️  Branch Switch Needed: gh-pages → main

💡 Next Steps:
   1. Switch to branch: git checkout main
   2. Accept task: python3 bin/branch_aware_coordinator.py worker accept BranchTask-def456
```

### Skill-Based Assignment
```
🎯 Branch-Aware Task Assignment Received!
=========================================
Task ID: BranchTask-ghi789
Description: Research OAuth2 security patterns
Priority: medium
Required Skills: research,security
✅ Skills match: research,security,analysis

💡 Next Steps:
   python3 bin/branch_aware_coordinator.py worker accept BranchTask-ghi789
```

## Timeout Handling

If no tasks are assigned within the timeout period:
```
⏰ No tasks received within timeout period

💡 Options:
   python3 bin/branch_aware_coordinator.py worker wait
   python3 bin/branch_aware_coordinator.py worker status
```

## Integration Features

- **Session Continuity**: Maintains session context across branch switches
- **Memory Tracking**: Updates worker status in memory graph
- **Real-time Coordination**: Uses memory monitor for efficient task detection
- **Branch Context**: Tracks branch switches and task assignments

## Cancellation

Use Ctrl+C to stop waiting:
```
🛑 Task waiting cancelled by user
   Worker remains registered and available
```

## Performance

- **Efficient Monitoring**: Uses memory monitor blocking instead of polling
- **Real-time Updates**: Immediate task assignment detection
- **Low Overhead**: Minimal resource usage while waiting
- **Scalable**: Supports multiple workers waiting simultaneously

## Next Steps

After receiving a task:
1. Accept task: `/flowloom:worker:accept [task-id]`
2. Check status: `/flowloom:worker:branch-status`
3. Complete task: `/flowloom:worker:complete [task-id]`