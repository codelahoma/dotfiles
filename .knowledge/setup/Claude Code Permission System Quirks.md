---
title: Claude Code Permission System Quirks
type: note
permalink: setup/claude-code-permission-system-quirks
tags:
- '#permissions'
- '#claude-code'
- '#security'
- '#quirks'
---

# Claude Code Permission System Quirks

## Understanding Permission Behavior

Claude Code's permission system has several important quirks that affect FlowLoom usage:

### Permission Syntax Patterns

1. **Bash Command Permissions**
   ```json
   "Bash(command:arguments)"
   ```
   - The colon separates command from arguments
   - Wildcards (`*`) allow any arguments
   - Example: `"Bash(git status:*)"` allows `git status` with any flags

2. **MCP Tool Permissions**
   ```json
   "mcp__server__function"
   ```
   - Double underscores separate namespace elements
   - No arguments needed in permission string
   - Example: `"mcp__filesystem__read_file"`

### Critical Quirks and Gotchas

#### 1. Wildcard Limitations
- `Bash(*:*)` does NOT allow all commands - it's invalid
- Each command must be explicitly listed
- Scripts need full path: `"Bash(./bin/script.sh:*)"` not `"Bash(script.sh:*)"`

#### 2. Path-Specific Permissions
FlowLoom uses multiple path formats for flexibility:
```json
"Bash(./bin/*:*)",                    // Current directory scripts
"Bash(/full/path/bin/*:*)",          // Absolute path scripts
"Bash(${FLOWLOOM_WORK_DIR}/bin/*:*)" // Environment variable paths
```

#### 3. Git Permission Exception
**NEVER** grant these permissions:
```json
"mcp__git__*"  // MCP git server - causes corruption
```
Instead, use standard bash git commands:
```json
"Bash(git add:*)",
"Bash(git commit:*)",
"Bash(git push:*)"
```

#### 4. Denial Takes Precedence
If a permission appears in both `allow` and `deny`:
- Denial wins
- No way to override
- Must remove from deny list

### FlowLoom-Specific Permissions

#### Essential FlowLoom Scripts
```json
"Bash(./bin/session_manager.sh:*)",
"Bash(./bin/format-footer.sh:*)",
"Bash(./bin/get_shell_pid.sh:*)"
```

#### Memory System Access
```json
"mcp__memory__create_entities",
"mcp__memory__create_relations",
"mcp__basic-memory__write_note"
```

#### Filesystem Operations
Always prefer MCP filesystem over built-in tools:
```json
"mcp__filesystem__read_file",     // Instead of Read tool
"mcp__filesystem__write_file",    // Instead of Write tool
"mcp__filesystem__edit_file"      // Instead of Edit tool
```

### Permission Prompts

When you see a permission prompt:
1. **Check the exact syntax** - Copy it exactly as shown
2. **Add to settings.local.json** - In the `.claude` directory
3. **Restart not required** - Changes apply to new conversations

### Security Best Practices

1. **Principle of Least Privilege**
   - Only grant what's needed
   - Start with FlowLoom defaults
   - Add permissions as required

2. **Review Before Granting**
   - Understand what the command does
   - Check if it's a FlowLoom command
   - Verify paths are within project

3. **Regular Audits**
   - Review permissions quarterly
   - Remove unused permissions
   - Update for new FlowLoom features

## Common Permission Sets

### Development Work
- All git operations
- Full filesystem access
- Memory system management
- FlowLoom scripts

### Code Review
- Read-only filesystem
- Git log and diff only
- Memory search only
- No write operations

### Documentation
- Markdown file writes
- Memory note creation
- No code execution
- No git operations

Remember: FlowLoom's installer provides optimized permission sets for common workflows. Start there and customize as needed.