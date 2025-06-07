# Register as branch-aware worker with cross-branch capabilities

Registers the current Claude instance as a branch-aware worker that can handle tasks on multiple branches with intelligent capability matching.

## Usage

```bash
python3 bin/branch_aware_coordinator.py worker register --branch gh-pages --capabilities web,content,documentation --preferences "main:capable;feature/*:can_switch"
```

## Parameters

- `--branch`: Primary branch (defaults to current git branch)
- `--capabilities`: Comma-separated worker capabilities
- `--preferences`: Cross-branch preferences (format: "branch:level;branch:level")

## Capability Categories

### Web Capabilities (gh-pages)
- `web`: HTML, CSS, JavaScript development
- `content`: Content creation and editing
- `documentation`: Technical writing and docs
- `design`: UI/UX design work

### Development Capabilities (main/feature)
- `development`: Code implementation
- `testing`: Unit, integration, and system testing
- `python`: Python programming expertise
- `security`: Security implementation and review
- `deployment`: CI/CD and deployment processes

### Research Capabilities (any branch)
- `research`: Information gathering and analysis
- `analysis`: Data analysis and interpretation
- `prototyping`: Rapid prototyping and experimentation

## Cross-Branch Preference Levels

- **preferred**: Worker's optimal branch, best performance
- **capable**: Worker can effectively work on this branch
- **can_switch**: Worker can switch to branch when needed

## Examples

### Web Worker (gh-pages focused)
```bash
python3 bin/branch_aware_coordinator.py worker register \
  --branch gh-pages \
  --capabilities web,content,documentation,html,css \
  --preferences "main:capable"
```

### Development Worker (main focused)
```bash
python3 bin/branch_aware_coordinator.py worker register \
  --branch main \
  --capabilities development,testing,python,git \
  --preferences "feature/*:capable;gh-pages:can_switch"
```

### Feature Worker (specialized)
```bash
python3 bin/branch_aware_coordinator.py worker register \
  --branch feature-auth \
  --capabilities research,development,security,prototyping \
  --preferences "main:capable"
```

### Full-Stack Worker (versatile)
```bash
python3 bin/branch_aware_coordinator.py worker register \
  --capabilities web,development,testing,documentation \
  --preferences "gh-pages:preferred;main:preferred;feature/*:capable"
```

## Automatic Branch Detection

If no branch is specified, the system automatically:
1. Detects current git branch
2. Determines session type (web/dev/feature)
3. Sets appropriate default cross-branch preferences

## Worker Session Types

Based on the primary branch:
- **web**: gh-pages branch, web content focus
- **dev**: main/master/develop branches, development focus
- **feature**: feature branches, specialized work focus

## Integration Features

- **Session Tracking**: Integrates with FlowLoom enhanced session manager
- **Memory Correlation**: Creates entities with Shell_ID tracking
- **Branch Context**: Maintains branch history and switch tracking
- **Real-time Status**: Updates availability and capabilities dynamically

## Registration Output

```
🤖 Branch-Aware Worker Registration
===================================
Worker ID: BranchWorker-12345
Primary Branch: gh-pages (web)
Capabilities: web,content,documentation

✅ Branch-aware worker registered successfully

🌲 Branch Capabilities:
   • gh-pages: web,content,documentation (preferred)
   • main: documentation,content (capable)

🔧 Session Context:
   Active Sessions: 3
   Session Type: web
   Integration: Enabled

💡 Next Steps:
   python3 bin/branch_aware_coordinator.py worker wait
   python3 bin/branch_aware_coordinator.py worker status
```

## Next Steps

After registration:
1. Wait for tasks: `/flowloom:worker:branch-wait`
2. Check status: `/flowloom:worker:branch-status`
3. Monitor coordination: `/flowloom:coord:branch-status`