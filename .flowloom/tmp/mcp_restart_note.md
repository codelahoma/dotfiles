# MCP Server Configuration Status

## Current State
- `.mcp.json` exists in project root with valid configuration for 4 servers:
  - filesystem (npx @modelcontextprotocol/server-filesystem)
  - memory (npx @modelcontextprotocol/server-memory) 
  - basic-memory (uvx basic-memory mcp, pointing to .flowloom/.knowledge)
  - github (npx @modelcontextprotocol/server-github)

## Issue
- Claude Code not discovering/loading MCP servers
- `claude mcp list` shows "No MCP servers configured"
- Suspected cause: `-c` option may be preventing proper MCP discovery

## What Was Tried
- Verified `.mcp.json` has correct permissions and valid JSON
- Fixed claude alias issue (was pointing to wrong executable)
- Ran `claude mcp reset-project-choices` successfully
- This should prompt for MCP server approval on next Claude Code startup

## Solution Found
- `.mcp.json` format wasn't being recognized by Claude Code
- Used `claude mcp add -s project` to manually configure all 4 servers
- All MCP servers now active and configured

## Repository Status
- All cleanup completed (node_modules removed, secrets cleaned from history)
- MCP servers now active and ready for development work