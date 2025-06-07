# Claude Code Commands

This directory contains custom slash commands for Claude Code that enhance your workflow with FlowLoom. These commands provide structured ways to plan, implement, document, and review code changes.

## 🚀 Auto-Detection System

**NEW**: Commands now auto-detect! You can use any of these patterns without the full slash syntax:

### FlowLoom Namespace Commands
- **Category:Command**: `plan:review`, `docs:daily`, `mode:config` (defaults to FlowLoom)
- **Explicit Namespace**: `flowloom:plan:review`, `flowloom:docs:daily`
- **Project Prefix**: `project:plan:review`, `project:docs:daily` (legacy support)
- **Slash Format**: `plan/review`, `docs/daily` (defaults to FlowLoom)
- **Mode Shortcuts**: `config`, `workflow`, `pair`, `documentation`

### Example Auto-Detection
```
Input: plan:review
Auto-detected: ✓ Loads flowloom/plan/review command

Input: flowloom:config  
Auto-detected: ✓ Loads flowloom/mode/config explicitly

Input: docs:daily yesterday
Auto-detected: ✓ Loads flowloom/docs/daily with "yesterday" argument

Input: config
Auto-detected: ✓ Mode shortcut to flowloom/mode/config

Input: Hello, how are you?
Auto-detected: ✗ Normal conversation continues
```

All patterns resolve to FlowLoom namespace commands while preserving user command space!

## Command Categories

### Documentation Commands (`/project:docs:*`)

Documentation commands help you create and maintain project documentation.

- `/project:docs:daily` - Generate a daily work summary report with git history analysis and create a Slack-ready summary that's automatically copied to your clipboard
- `/project:docs:change` - Document code changes with detailed explanations
- `/project:docs:after` - Create post-implementation documentation

### Git Commands (`/project:git:*`)

Git commands help you manage your Git workflow.

- `/project:git:backup` - Create a timestamped backup of your current branch
- `/project:git:cleanup-backups` - Find and remove all backup branches for a specific branch

### Planning Commands (`/project:plan:*`)

Planning commands help you create structured plans for implementation.

- `/project:plan:arch` - Create an architecture plan
- `/project:plan:impl` - Create an implementation plan
- `/project:plan:mem` - Manage plan memory
- `/project:plan:review` - Review an existing plan
- `/project:plan:update` - Update an existing plan

### Implementation Commands (`/project:impl:*`)

Implementation commands guide you through implementing code changes.

- `/project:impl:plan` - Create a step-by-step implementation plan
- `/project:impl:subsection` - Create a detailed implementation plan for a subsection
- `/project:impl:lint` - Lint code and fix issues
- `/project:impl:commit` - Create a well-formatted commit message
- `/project:impl:complete` - Wrap up a task with staging, linting, commits, and plan updates

### Review Commands (`/project:rev:*`)

Review commands help with code review and pull requests.

- `/project:rev:pr` - Generate pull request descriptions

### Mode Commands (`/project:mode:*`)

Mode commands switch Claude into specialized operating modes.

- `/project:mode:workflow` - Enter Feature Development Workflow Mode (follows structured workflow steps)
- `/project:mode:pair` - Enter Pair Programming Mode
- `/project:mode:command_dev` - Enter Command Development Mode
- `/project:mode:code_review` - Enter Code Review Mode
- `/project:mode:documentation` - Enter Documentation Mode

### Miscellaneous Commands

- `/project:architectures` - Explain and compare architectural patterns
- `/project:approach` - Get guidance on approaching a problem
- `/project:lint` - Help with linting and code quality
- `/project:pr` - Create a pull request description
- `/project:update` - Update an existing file
- `/project:plans` - List and manage plans
- `/project:implementation-plan` - Create a detailed implementation plan
- `/project:group-and-commit` - Group changes and create a commit message
- `/project:change-walkthrough` - Walk through code changes step by step
- `/project:report_daily_summary` - Generate a daily work summary
- `/project:report_after_hours` - Report after-hours work

## Command Directory Structure

```
.claude/
├── README.md                   # This file - comprehensive guide to commands
├── commands/                   # All available slash commands
│   ├── docs/                   # Documentation commands
│   │   ├── daily.md            # Daily report generator
│   │   ├── change.md           # Document code changes
│   │   └── after.md            # Post-implementation documentation
│   ├── git/                    # Git commands
│   │   ├── backup.md           # Create branch backup
│   │   └── cleanup-backups.md  # Remove branch backups
│   ├── impl/                   # Implementation commands
│   │   ├── plan.md             # Create implementation plan
│   │   ├── lint.md             # Lint code
│   │   ├── commit.md           # Format commit messages
│   │   ├── subsection.md       # Subsection implementation plan
│   │   └── complete.md         # Task completion workflow
│   ├── mode/                   # Mode-switching commands
│   │   ├── workflow.md         # Feature development workflow mode
│   │   ├── pair.md             # Pair programming mode
│   │   ├── command_dev.md      # Command development mode
│   │   ├── code_review.md      # Code review mode
│   │   └── documentation.md    # Documentation mode
│   ├── plan/                   # Planning commands
│   │   ├── arch.md             # Architecture planning
│   │   ├── impl.md             # Implementation planning
│   │   ├── mem.md              # Plan memory management
│   │   ├── review.md           # Plan review
│   │   └── update.md           # Plan updates
│   ├── rev/                    # Review commands
│   │   └── pr.md               # PR description generation
│   └── [other standalone command files]
```

## Using Commands

### Auto-Detection Method (Recommended)
Simply type command patterns naturally - they'll be auto-detected and executed:

```
plan:review                    # Auto-detected ✓
docs:daily yesterday           # Auto-detected ✓  
config                         # Auto-detected ✓
project:mode:workflow          # Auto-detected ✓
impl/commit                    # Auto-detected ✓
```

### Traditional Slash Method
You can still use the full slash syntax:

```
/project:docs:daily yesterday
/project:plan:review
/project:mode:config
```

### Supported Auto-Detection Patterns
- **`category:command [args]`** → `plan:review`, `docs:daily today`
- **`project:category:command [args]`** → `project:mode:config` 
- **`category/command [args]`** → `plan/review`, `impl/commit`
- **`mode_name`** → `config`, `workflow`, `pair`, `documentation`

Commands take arguments naturally. For detailed usage instructions, refer to the specific command file.

## Command Development

When creating or modifying commands:

1. **Place commands in appropriate subdirectories** based on their category
2. **Use descriptive filenames** that match the command's purpose
3. **Include detailed instructions** in the command file
4. **Test auto-detection patterns** - ensure your command works with `category:command` syntax
5. **Update this README.md** when adding new commands
6. **Update the commands cheatsheet** at `docs/commands-cheatsheet.md`

### Auto-Detection Integration
New commands automatically work with auto-detection if they follow the standard structure:
- File path: `.claude/commands/[category]/[command].md`
- Auto-detected as: `category:command`, `project:category:command`, `category/command`
- Mode commands also work as single-word shortcuts: `config`, `workflow`, etc.

## Command Best Practices

- **Single responsibility** - Each command should have a clear, focused purpose
- **Detailed instructions** - Include comprehensive guidance for Claude Code within each command file
- **Argument processing** - Use `$ARGUMENTS` and follow the argument processor pattern
- **Consistent formatting** - Structure output in a user-friendly, consistent format
- **Auto-detection friendly** - Use clear category/command naming that works naturally