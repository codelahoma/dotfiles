# Dispatch branch-aware task to optimal worker

Creates and assigns a task to the best available worker based on branch requirements, skills, and availability.

## Usage

```bash
python3 bin/branch_aware_coordinator.py controller dispatch "Update homepage content" --requires-branch gh-pages --requires-skills web,content
```

## Parameters

- `description`: Task description (required)
- `--requires-branch`: Specific branch required for task execution
- `--requires-skills`: Comma-separated list of required skills
- `--priority`: Task priority (low, medium, high, critical)
- `--no-cross-branch`: Disable cross-branch assignment (strict branch matching)

## Branch-Aware Assignment

The system automatically:
1. **Finds workers** on the required branch or with cross-branch capability
2. **Scores workers** based on branch match, skills, availability, and preferences
3. **Assigns optimally** to the highest-scoring available worker
4. **Tracks coordination** in memory graph with Shell_ID correlation

## Worker Scoring Algorithm

Workers are scored (0-100) based on:
- **Branch match** (50 points): Perfect branch match
- **Cross-branch capability** (20-40 points): Based on preference level
- **Skill match** (30 points): Percentage of required skills possessed
- **Availability** (10 points): Currently available workers preferred
- **Priority bonus** (5-10 points): High/critical priority tasks

## Examples

### Web Content Task (gh-pages)
```bash
python3 bin/branch_aware_coordinator.py controller dispatch \
  "Update website homepage with new branding" \
  --requires-branch gh-pages \
  --requires-skills web,content,html \
  --priority high
```

### Development Task (main)
```bash
python3 bin/branch_aware_coordinator.py controller dispatch \
  "Implement user authentication API" \
  --requires-branch main \
  --requires-skills development,python,security \
  --priority critical
```

### Research Task (any branch)
```bash
python3 bin/branch_aware_coordinator.py controller dispatch \
  "Research OAuth2 best practices" \
  --requires-skills research,security \
  --priority medium
```

### Strict Branch Assignment
```bash
python3 bin/branch_aware_coordinator.py controller dispatch \
  "Deploy to production" \
  --requires-branch main \
  --requires-skills deployment,ops \
  --no-cross-branch \
  --priority critical
```

## Cross-Branch Workflows

The system supports complex workflows spanning multiple branches:

1. **Feature → Main → gh-pages**: Development pipeline
2. **Research → Feature → Main**: Research to implementation
3. **Main ↔ gh-pages**: Documentation synchronization

## Assignment Types

- **perfect_match**: Worker on exact required branch
- **cross_branch_preferred**: Worker prefers the required branch
- **cross_branch_capable**: Worker capable on required branch
- **cross_branch_switch**: Worker can switch to required branch
- **any_branch**: No branch requirement specified

## Integration

- **Memory Tracking**: Task and assignment tracked in memory graph
- **Session Correlation**: Integrated with FlowLoom session management
- **Worker Updates**: Automatic status updates for assigned workers

## Next Steps

After task dispatch:
1. Monitor status: `/flowloom:coord:branch-status`
2. Wait for completion: `/flowloom:coord:wait-for [task-id]`
3. View results: `/flowloom:coord:results [task-id]`