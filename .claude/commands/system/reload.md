Let input_args = "$ARGUMENTS"

# Reload Claude Configuration

Reload Claude's configuration files and command structure without restarting the session.

## What This Command Does

1. **Re-read Configuration Files**
   - CLAUDE.md (project-wide instructions)
   - CLAUDE.local.md (user-specific settings)
   - .claude/settings.json and .claude/settings.local.json
   - .mcp.json (MCP server configurations)

2. **Refresh Command Structure**
   - Scan .claude/commands/ directory for new/modified commands
   - Update command categories and availability
   - Refresh argument processing patterns

3. **Apply Updated Settings**
   - Tool permissions and allowlists
   - Environment variables
   - MCP server connections
   - Mode configurations

## Argument Interpretation

First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Full configuration reload
- If input_args is "commands": Focus on command structure only
- If input_args is "config": Focus on configuration files only
- If input_args is "mcp": Focus on MCP server configurations only

## Argument Patterns
- (no arguments) - Full reload of all configuration
- `commands` - Reload command structure only
- `config` - Reload CLAUDE.md and settings files only
- `mcp` - Reload MCP server configurations only

## Implementation

Based on the arguments, perform the appropriate reload:

### Full Reload (default)
1. Display current configuration status
2. Re-read all CLAUDE.md files
3. Refresh command structure from .claude/commands/
4. Reload settings files
5. Refresh MCP connections
6. Confirm successful reload

### Commands-only Reload
1. Scan .claude/commands/ directory
2. Update command categories and paths
3. Refresh argument processing
4. Display updated command structure

### Config-only Reload
1. Re-read CLAUDE.md and CLAUDE.local.md
2. Reload .claude/settings files
3. Apply updated tool permissions
4. Display configuration summary

### MCP-only Reload
1. Re-read .mcp.json
2. Refresh MCP server connections
3. Display MCP server status

## Output Format

Provide a clear summary of what was reloaded:

```
Configuration Reload Complete ✅

Reloaded:
- CLAUDE.md: [status]
- CLAUDE.local.md: [status]
- Commands: [count] commands in [count] categories
- Settings: [status]
- MCP Servers: [count] servers

Changes Detected:
- [List any changes found]

Ready to use updated configuration.
```

## Important Notes

- This simulates a configuration reload by re-processing the files
- Any syntax errors in configuration files will be reported
- New commands become immediately available
- Modified command behavior takes effect immediately
- MCP server changes may require actual restart for some servers

## Claude Code TUI Limitation

⚠️ **Known Limitation**: The Claude Code TUI dropdown menu is populated at startup and will not refresh to show new commands added during the session. However:

✅ **What Works**: Existing commands in the dropdown will always reflect their current implementation after reload
✅ **Auto-Detection**: New commands are still available via auto-detection patterns (e.g., `category:command`)
✅ **Manual Access**: You can always type commands manually or use the commands cheatsheet

This makes reload particularly valuable for:
- Command development and iteration on existing commands
- Configuration changes and MCP server updates
- Testing new command behaviors and argument patterns