# Show branch-aware worker status and capabilities

Displays comprehensive status of the current worker including branch capabilities, task assignments, and cross-branch coordination context.

## Usage

```bash
python3 bin/branch_aware_coordinator.py worker status
```

## Status Information

### Worker Identity
- Worker ID and current status
- Current git branch and session type
- Registration status and capabilities

### Branch Capabilities
- Primary branch and preference level
- Cross-branch capabilities and preferences
- Skill set and specializations

### Task Assignment Status
- Current task assignments
- Task history and completion count
- Branch requirements for assigned tasks

### Branch Context
- Current vs registered branch
- Branch switch tracking
- Session integration status

## Example Output

### Available Worker
```
🔍 Branch-Aware Worker Status
============================
Worker ID: BranchWorker-12345
Current Branch: gh-pages (web)

✅ Worker Status: available
   Capabilities: web,content,documentation
   Current Task: none
   Tasks Completed: 3
   Last Heartbeat: 2025-05-29T23:45:00Z

🌲 Branch Capabilities:
   • gh-pages: web,content,documentation (preferred)
   • main: documentation,content (capable)
   • feature/*: content (can_switch)

📋 No assigned tasks
```

### Busy Worker
```
🔍 Branch-Aware Worker Status
============================
Worker ID: BranchWorker-67890
Current Branch: main (dev)

🔄 Worker Status: busy
   Capabilities: development,testing,python
   Current Task: BranchTask-abc123
   Tasks Completed: 5
   Last Heartbeat: 2025-05-29T23:47:30Z

🌲 Branch Capabilities:
   • main: development,testing,python (preferred)
   • feature/*: development,testing (capable)
   • gh-pages: documentation (can_switch)

📋 Assigned Tasks: 1
   • BranchTask-abc123: in_progress (branch: main)
```

### Cross-Branch Assignment
```
🔍 Branch-Aware Worker Status
============================
Worker ID: BranchWorker-54321
Current Branch: gh-pages (web)

🔄 Worker Status: busy
   Capabilities: web,content,documentation
   Current Task: BranchTask-def456
   Tasks Completed: 2

🌲 Branch Capabilities:
   • gh-pages: web,content,documentation (preferred)
   • main: documentation (capable)

📋 Assigned Tasks: 1
   • BranchTask-def456: in_progress (branch: main) 🌉

⚠️  Branch Status:
   Registered on: gh-pages
   Currently on: gh-pages
   Task requires: main branch
   🌉 Cross-branch assignment active
```

## Status Indicators

- ✅ **Available**: Ready for task assignments
- 🔄 **Busy**: Currently executing assigned tasks
- ❌ **Offline**: Worker disconnected or unavailable
- 🌉 **Cross-Branch**: Working on task requiring different branch

## Branch Mismatch Detection

The system detects and reports:
- **Registration Branch**: Branch worker was registered on
- **Current Branch**: Branch worker is currently on
- **Task Requirements**: Branch requirements for assigned tasks
- **Switch Recommendations**: Suggestions for branch alignment

## Not Registered Status

If worker is not registered:
```
❌ Worker not registered
   Run: python3 bin/branch_aware_coordinator.py worker register

💡 Registration options:
   # Current branch (gh-pages)
   python3 bin/branch_aware_coordinator.py worker register --branch gh-pages

   # With cross-branch preferences
   python3 bin/branch_aware_coordinator.py worker register --branch gh-pages --preferences 'main:capable;feature/*:can_switch'
```

## Integration Status

- **Session Integration**: Shows FlowLoom session manager connection
- **Memory Tracking**: Confirms entity status in memory graph
- **Branch Coordination**: Displays coordination session participation

## Performance Metrics

- **Tasks Completed**: Total number of finished tasks
- **Branch Switches**: Number of cross-branch assignments handled
- **Availability**: Current readiness for new assignments
- **Response Time**: Heartbeat and communication status

## Troubleshooting

### Common Issues
1. **Branch Mismatch**: Current branch differs from task requirements
2. **Capability Gap**: Assigned tasks require unavailable skills
3. **Communication Lost**: Heartbeat or memory connection issues

### Solutions
1. **Branch Switch**: Use `git checkout [required-branch]`
2. **Re-register**: Update worker capabilities if needed
3. **Restart**: Re-register worker to restore connection

## Related Commands

- `/flowloom:worker:branch-register`: Register or update worker
- `/flowloom:worker:branch-wait`: Wait for new task assignments
- `/flowloom:coord:branch-status`: View overall coordination status
- `/flowloom:coord:branch-workers`: See all workers