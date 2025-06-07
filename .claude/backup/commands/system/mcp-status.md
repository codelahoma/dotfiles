Let input_args = "$ARGUMENTS"

# MCP Server Status Check

Display detailed status and diagnostics for all MCP (Model Context Protocol) servers.

## What This Command Does

1. **Server Connection Status**
   - Check which servers are connected and responding
   - Display connection health and response times
   - Report any connection errors or timeouts

2. **Server Capabilities**
   - List available tools and functions for each server
   - Show server-specific configuration and scopes
   - Display server versions and metadata

3. **Configuration Analysis**
   - Validate .mcp.json configuration
   - Check environment variable requirements
   - Report any configuration issues

4. **Performance Metrics**
   - Show server response times
   - Display usage statistics
   - Report any performance issues

## Argument Interpretation

First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Full MCP server status
- If input_args is a server name: Status for specific server only
- If input_args is "config": Configuration analysis only
- If input_args is "tools": List all available tools
- If input_args is "test": Test all server connections

## Argument Patterns
- (no arguments) - Complete MCP server status
- `memory` - Status for memory server only
- `filesystem` - Status for filesystem server only
- `github` - Status for GitHub server only
- `config` - Configuration validation only
- `tools` - List all MCP tools and capabilities
- `test` - Test server connections and response times

## Implementation

Based on the arguments, provide detailed MCP diagnostics:

### Read MCP Configuration
First, load and validate the .mcp.json configuration file.

### Test Server Connections
For each configured server, attempt to connect and verify functionality.

### Display Server Status
Show comprehensive status for each server including:
- Connection status (✅ Connected, ⚠️ Degraded, ❌ Disconnected)
- Available tools and functions
- Configuration details
- Performance metrics
- Any error messages

### Configuration Validation
Check for:
- Valid JSON syntax in .mcp.json
- Required environment variables
- Proper server command paths
- Configuration consistency

## Output Format

Provide a detailed MCP server dashboard:

```
MCP Server Status Dashboard 🔌

📋 Configuration
- File: .mcp.json ✅ Valid
- Servers: 6 configured
- Environment: All variables set

🔌 Server Status
┌─────────────────┬──────────┬───────────┬──────────────┐
│ Server          │ Status   │ Tools     │ Response     │
├─────────────────┼──────────┼───────────┼──────────────┤
│ memory          │ ✅ Active │ 9 tools   │ <100ms       │
│ filesystem      │ ✅ Active │ 11 tools  │ <50ms        │
│ github          │ ✅ Active │ 23 tools  │ <200ms       │
│ sqlite          │ ✅ Active │ 5 tools   │ <75ms        │
│ basic-memory    │ ✅ Active │ 8 tools   │ <150ms       │
│ puppeteer       │ ✅ Active │ 7 tools   │ <300ms       │
└─────────────────┴──────────┴───────────┴──────────────┘

🛠️ Available Tools (63 total)
Memory: create_entities, add_observations, read_graph, search_nodes...
Filesystem: read_file, write_file, edit_file, list_directory...
GitHub: create_issue, create_pull_request, search_repositories...
[Additional tools...]

⚠️ Issues Detected
- None

💡 Performance Tips
- All servers responding within acceptable limits
- Consider enabling caching for GitHub API calls
```

## Troubleshooting Guidance

When issues are detected, provide specific solutions:
- **Connection failures**: Check server installation and paths
- **Missing tools**: Verify server versions and capabilities
- **Performance issues**: Suggest optimization strategies
- **Configuration errors**: Provide specific fix instructions

## Server-Specific Diagnostics

For each server type, show relevant details:
- **Memory**: Entity count, relation count, recent activity
- **Filesystem**: Allowed directories, file operation permissions
- **GitHub**: Token status, rate limit status, repository access
- **SQLite**: Database file status, table count, query performance
- **Basic-memory**: Knowledge directory status, document count
- **Puppeteer**: Browser status, navigation capabilities

This command is essential for diagnosing MCP server issues and ensuring optimal performance.