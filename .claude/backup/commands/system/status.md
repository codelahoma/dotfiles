Let input_args = "$ARGUMENTS"

# System Status Check

Display comprehensive system status including configuration, commands, and MCP servers.

## What This Command Does

1. **Configuration Status**
   - CLAUDE.md and CLAUDE.local.md presence and validity
   - Settings files (.claude/settings.json, .claude/settings.local.json)
   - Active mode and session information

2. **Command System Status**
   - Available command categories and counts
   - Recently modified commands
   - Command structure health

3. **MCP Server Status**
   - Connected servers and their status
   - Server configurations and capabilities
   - Connection health

4. **Memory System Status**
   - Memory graph statistics
   - Knowledge base status
   - Recent activity summary

## Argument Interpretation

First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Full system status
- If input_args is "config": Configuration status only
- If input_args is "commands": Command system status only
- If input_args is "mcp": MCP server status only
- If input_args is "memory": Memory system status only

## Argument Patterns
- (no arguments) - Complete system status
- `config` - Configuration files and settings only
- `commands` - Command structure and availability only
- `mcp` - MCP server connections and status only
- `memory` - Memory and knowledge systems only

## Implementation

Display a comprehensive status dashboard:

### Configuration Status
- Check CLAUDE.md and CLAUDE.local.md files
- Verify settings files exist and are valid JSON
- Show active permissions and tool allowlists
- Display current working directory and git branch

### Command System Status
- Count available commands by category
- Show command structure health
- List any missing or broken command files
- Display recently modified commands

### MCP Server Status
- List configured servers from .mcp.json
- Show connection status for each server
- Display server capabilities and tools
- Report any connection errors

### Memory System Status
- Show memory.json statistics (entities, relations, observations)
- Display knowledge base (.knowledge/) status
- Show recent memory activity
- Report any memory system issues

## Output Format

Provide a clean, organized status dashboard:

```
FlowLoom System Status ✅

📁 Configuration
- CLAUDE.local.md: ✓ Loaded (2.1KB)
- Settings: ✓ Valid (45 permissions)
- Working Dir: /Users/rodk/github/flowloom
- Git Branch: main

🔧 Commands
- Categories: 12 (89 total commands)
- System: 3 commands
- Status: ✓ All commands valid

🔌 MCP Servers  
- memory: ✅ Connected (Create/Read/Update/Delete)
- filesystem: ✅ Connected (File Operations)
- github: ✅ Connected (Repository Management)
- [Additional servers...]

🧠 Memory Systems
- Graph: 156 entities, 89 relations, 234 observations
- Knowledge: 23 documents in .knowledge/
- Recent: 5 activities in last 24h

⚡ Quick Actions
1. Reload configuration: /system:reload
2. Check MCP details: /system:mcp-status  
3. View memory stats: /memory:stats
```

## Health Indicators

Use clear visual indicators:
- ✅ **Healthy/Connected**
- ⚠️ **Warning/Degraded** 
- ❌ **Error/Disconnected**
- 📊 **Statistics/Info**

## Troubleshooting Hints

When issues are detected, provide specific guidance:
- Configuration file syntax errors
- Missing command files or categories
- MCP server connection problems
- Memory system inconsistencies