# Initialize branch-aware coordination session

This command initializes a branch-aware Multi-Claude coordination session that can handle workers on different branches and assign tasks based on branch requirements.

## Usage

```bash
python3 bin/branch_aware_coordinator.py controller init --session web-development --objective "Build new website features"
```

## Parameters

- `--session`: Optional session name (auto-generated if not provided)
- `--objective`: Session objective description
- `--strategy`: Branch coordination strategy (adaptive, strict, flexible)

## Branch Strategy Options

- **adaptive**: Automatically assigns tasks to best available workers, allows cross-branch
- **strict**: Only assigns tasks to workers on exact required branches
- **flexible**: Prefers branch matches but allows any worker if needed

## Examples

### Web Development Session
```bash
python3 bin/branch_aware_coordinator.py controller init \
  --session web-project-v2 \
  --objective "Implement new homepage and documentation" \
  --strategy adaptive
```

### Feature Development Session
```bash
python3 bin/branch_aware_coordinator.py controller init \
  --session auth-system \
  --objective "Build complete authentication system" \
  --strategy strict
```

## Integration

- **Session Management**: Integrates with FlowLoom enhanced session manager
- **Memory Tracking**: Creates entities in memory graph with Shell_ID correlation
- **Branch Context**: Tracks controller branch and session type (web/dev/feature)

## Next Steps

After initialization:
1. Register workers: `/flowloom:coord:branch-workers`
2. Dispatch tasks: `/flowloom:coord:branch-dispatch`
3. Monitor status: `/flowloom:coord:branch-status`