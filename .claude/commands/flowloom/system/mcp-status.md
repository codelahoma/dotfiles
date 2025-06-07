Let input_args = "$ARGUMENTS"

You are helping the user check and monitor their MCP (Model Context Protocol) server status, configuration, and capabilities.

**Purpose**: Provide comprehensive visibility into MCP server health, connectivity, and configuration for optimal FlowLoom performance.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Show comprehensive MCP status overview
- If input_args is "detailed": Show detailed server capabilities and configuration
- If input_args is "config": Show only configuration files and settings
- If input_args is "permissions": Show current permission settings
- If input_args matches a server name: Show detailed info for that specific server

## MCP Status Check Process

1. **Check MCP Server Connectivity**
   Use the mcp__filesystem__list_allowed_directories tool to verify filesystem MCP is working
   Use the mcp__memory__read_graph tool to verify memory MCP is working
   Use the mcp__git__git_status tool to verify git MCP is working

2. **Review Configuration Files**
   Read and analyze:
   - .mcp.json (project-level MCP servers)
   - .claude/settings.local.json (permissions and local settings)
   - Any other relevant configuration files

3. **Server Capabilities Summary**
   For each connected server, provide:
   - **memory**: Knowledge graph management, entity creation, search
   - **filesystem**: File operations, directory traversal, search within allowed paths
   - **brave-search**: Web search, current information retrieval
   - **git**: Git operations, repository management
   - **sqlite**: Database operations on .flowloom.db
   - **github**: Repository management, issue/PR creation
   - **basic-memory**: Enhanced memory with note management

4. **Permission Analysis**
   Review current permissions and identify:
   - Which MCP functions are allowed
   - Any potentially missing permissions
   - Security considerations

5. **Integration Recommendations**
   Based on available servers, suggest:
   - Command improvements that could leverage MCP capabilities
   - Workflow optimizations
   - Configuration adjustments

Show a comprehensive status report including server connectivity, configuration health, and optimization recommendations. If any servers are not responding, provide troubleshooting guidance.