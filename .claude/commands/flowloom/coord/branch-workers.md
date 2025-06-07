# List branch-aware workers with capabilities

Shows all registered branch-aware workers, their current branches, capabilities, and cross-branch preferences.

## Usage

```bash
python3 bin/branch_aware_coordinator.py controller workers --all-branches
```

## Parameters

- `--all-branches`: Show workers from all branches (default: current branch focus)

## Output

Workers are grouped by branch with the following information:
- Worker ID and status
- Current branch and session type (web/dev/feature)
- Branch capabilities and preferences
- Current task assignment
- Cross-branch capability indicators

## Example Output

```
🌲 gh-pages branch (web workers):
   ✅ BranchWorker-12345
      Status: available
      Branch Capabilities:
        • gh-pages: web,content,documentation (preferred)
        • main: documentation,content (capable)

🌲 main branch (dev workers):
   ✅ BranchWorker-67890
      Status: busy
      Current Task: BranchTask-abc123
      Branch Capabilities:
        • main: development,testing,python (preferred)
        • feature/*: development,testing (capable)
        • gh-pages: documentation (can switch)
```

## Branch Types

- **Web workers** (gh-pages): Specialized in content, documentation, HTML/CSS
- **Dev workers** (main/master): Focused on development, testing, implementation
- **Feature workers** (feature branches): Research, prototyping, specialized features

## Worker Capability Levels

- **preferred**: Worker's primary branch, optimal performance
- **capable**: Worker can effectively work on this branch
- **can_switch**: Worker can switch to branch if needed

## Integration

- Uses enhanced session management for worker context
- Memory graph tracking for real-time status
- Cross-branch coordination supported

## Next Steps

After reviewing workers:
1. Dispatch tasks: `/flowloom:coord:branch-dispatch`
2. Monitor status: `/flowloom:coord:branch-status`