Let input_args = "$ARGUMENTS"

## Auto-Detection Command Processor

This command automatically detects when user input matches a slash command pattern and loads the appropriate command using the slashload mechanism.

## Detection Process

Analyze the user input (input_args) for command-like patterns:

### Pattern 1: Category:Command Format
- Pattern: `[category]:[command] [optional_args]`
- Examples: `plan:review`, `docs:daily yesterday`, `mode:config`
- Action: Convert to `slashload [category]/[command] [optional_args]`

### Pattern 2: Project:Category:Command Format  
- Pattern: `project:[category]:[command] [optional_args]`
- Examples: `project:mode:config`, `project:plan:review FlowLoom/225`
- Action: Convert to `slashload [category]/[command] [optional_args]`

### Pattern 3: Mode Name Shortcuts
- Pattern: Single word matching known mode names
- Examples: `config`, `workflow`, `documentation`, `pair`
- Action: Convert to `slashload mode/[mode_name]`

### Pattern 4: Command File Paths
- Pattern: `[category]/[command] [optional_args]`
- Examples: `plan/review`, `docs/daily today`
- Action: Convert to `slashload [category]/[command] [optional_args]`

## Implementation Steps

**Step 1: Analyze Input Pattern**

First, examine input_args to determine if it matches any command patterns:

1. **Check for colon-separated patterns**: `category:command` or `project:category:command`
2. **Check for slash-separated patterns**: `category/command`
3. **Check for single-word mode names**: Match against known mode names
4. **Extract arguments**: Separate command from arguments

**Step 2: Pattern Matching Logic**

```
If input_args matches pattern "[category]:[command] [args]":
  - Extract category, command, and remaining args
  - Validate category exists in known categories
  - Execute: slashload [category]/[command] [args]

If input_args matches pattern "project:[category]:[command] [args]":
  - Extract category, command, and remaining args  
  - Validate category exists in known categories
  - Execute: slashload [category]/[command] [args]

If input_args matches pattern "[category]/[command] [args]":
  - Extract category, command, and remaining args
  - Validate category exists in known categories  
  - Execute: slashload [category]/[command] [args]

If input_args is single word matching known mode:
  - Execute: slashload mode/[mode_name]

Otherwise:
  - No command detected, continue normal conversation
```

**Step 3: Validation and Execution**

Before executing slashload:
1. Verify the category exists in known command categories
2. Check that the combination makes sense (don't need to check file existence - slashload will handle that)
3. Preserve all arguments after the command name
4. Execute the slashload command with proper argument passing

## Known Command Categories
- `docs` - Documentation commands (daily, change, after)
- `impl` - Implementation commands (commit, complete, lint)  
- `plan` - Planning commands (review, update, arch, highlevel, detailed, subsection, mem)
- `rev` - Review commands (pr)
- `mode` - Mode switching commands (select, config, workflow, pair, documentation, etc.)
- `git` - Git workflow commands (backup, cleanup-backups, sync-commands)
- `files` - File management commands (cleanup, move, organize, structure)
- `specs` - Specification commands (review)

## Known Mode Names
- `config`, `workflow`, `pair`, `documentation`, `code_review`, `files`, `specification`, `story`, `select`

## Response Format

If command detected and executed:
```
🔄 **Auto-detected command**: [detected_pattern]
📂 **Loading**: slashload [category]/[command] [args]

[Command output follows]
```

If no command detected:
```
❌ **No command detected** in: "[input_args]"
💬 **Continuing normal conversation**
```

## Error Handling

- If pattern matches but file doesn't exist, report missing command
- If category exists but command doesn't, suggest available commands in that category
- If input is ambiguous, ask for clarification

## Testing Patterns

Test these inputs should auto-detect:
- `plan:review` → `slashload plan/review`
- `project:mode:config` → `slashload mode/config`
- `docs:daily yesterday` → `slashload docs/daily yesterday`
- `config` → `slashload mode/config`
- `workflow` → `slashload mode/workflow`
- `plan/review FlowLoom/225` → `slashload plan/review FlowLoom/225`

Test these should NOT auto-detect:
- `Hello, how are you?`
- `Can you help me with Python?`
- `What is the meaning of life?`