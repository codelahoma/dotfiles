# Show comprehensive branch-aware coordination status

Displays detailed status of the branch-aware coordination system including workers, tasks, cross-branch activities, and session integration.

## Usage

```bash
python3 bin/branch_aware_coordinator.py controller status
```

## Status Information

### Controller Status
- Current branch and session type
- Shell PID and active sessions count
- Session manager integration status

### Branch Coordination
- Active coordination sessions
- Registered workers by branch
- Total tasks and their distribution
- Cross-branch coordination statistics

### Branch Distribution
Shows worker distribution across branches:
- **gh-pages (web)**: Content, documentation, web workers
- **main (dev)**: Development, testing, implementation workers  
- **feature branches**: Research, prototyping, specialized workers

### Task Status Breakdown
- **Pending**: Tasks waiting for assignment
- **In Progress**: Tasks currently being executed
- **Completed**: Finished tasks with results
- **Cross-Branch**: Tasks assigned across different branches

## Example Output

```
📊 Branch-Aware Coordination Status
====================================

🎯 Controller Status:
   Branch: main (dev)
   Shell PID: 12345
   Active Sessions: 3

🌳 Branch Coordination:
   Active Sessions: 1
   Registered Workers: 4
   Total Tasks: 7

🌲 Branch Distribution:
   • gh-pages (web): 2 workers
   • main (dev): 1 worker
   • feature-auth (feature): 1 worker

📋 Task Status:
   Pending: 2
   In Progress: 3
   Completed: 2

🌉 Cross-Branch Coordination:
   Active Cross-Branch Tasks: 2
```

## Cross-Branch Activity

The status shows how many tasks are currently assigned to workers on different branches than required, indicating:
- **Active coordination** between branches
- **Workflow complexity** and branch dependencies
- **Resource utilization** across the development environment

## Integration Status

- **Session Management**: Shows FlowLoom session integration
- **Memory Graph**: Confirms entity tracking and correlation
- **Branch Context**: Displays branch-aware session types

## Performance Indicators

- **Worker availability** across branches
- **Task distribution** efficiency
- **Cross-branch coordination** success rate
- **Session integration** health

## Related Commands

- `/flowloom:coord:branch-workers`: Detailed worker status
- `/flowloom:coord:branch-dispatch`: Create new tasks
- `/flowloom:worker:branch-register`: Register new workers
- `/flowloom:coord:wait-for`: Wait for task completion