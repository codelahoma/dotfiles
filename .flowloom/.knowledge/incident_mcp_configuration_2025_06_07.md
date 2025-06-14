# Incident: MCP Server Configuration Issue

**Date:** 2025-06-07  
**Type:** Configuration  
**Status:** Resolved  
**Severity:** Medium  

## Summary
MCP servers configured in `.mcp.json` were not being discovered by Claude Code, requiring manual configuration via CLI commands.

## Root Cause
The `.mcp.json` file format was not being properly recognized by Claude Code, despite having valid JSON configuration for 4 MCP servers.

## Timeline
1. **Initial State**: `.mcp.json` existed with valid configuration for filesystem, memory, basic-memory, and github servers
2. **Problem Discovery**: `claude mcp list` showed "No MCP servers configured"
3. **Attempted Solution**: Reset project choices with `claude mcp reset-project-choices`
4. **Failed Resolution**: Restarting Claude Code did not prompt for MCP server approval
5. **Successful Resolution**: Manually added all 4 servers using `claude mcp add -s project`

## Commands Used
```bash
claude mcp add -s project filesystem "npx -y @modelcontextprotocol/server-filesystem"
claude mcp add -s project memory "npx -y @modelcontextprotocol/server-memory"
claude mcp add -s project basic-memory "uvx basic-memory mcp" -e BASIC_MEMORY_PROJECT_PATH="/Users/rodk/.homesick/repos/dotfiles/.flowloom/.knowledge"
claude mcp add -s project github "npx -y @modelcontextprotocol/server-github"
```

## Resolution
All MCP servers are now active and configured. The `.mcp.json` file was automatically updated with the correct format including `"type": "stdio"` fields.

## Prevention
- Use `claude mcp add` commands for reliable MCP server configuration
- Verify configuration with `claude mcp list` after setup
- Document working MCP configurations for future reference

## Impact
- FlowLoom memory system now accessible
- Enhanced file system operations available
- GitHub integration active
- Development workflow fully operational