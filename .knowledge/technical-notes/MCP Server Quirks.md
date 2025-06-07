---
title: MCP Server Quirks
type: note
permalink: technical-notes/mcp-server-quirks
---

# MCP Server Quirks

**Date**: May 27, 2025  
**Session**: session-5189-website  
**Context**: Discovered while fixing GitHub Actions regex errors

## Key Discovery: Relative Paths Enable MCP Filesystem Tools

When working in FlowLoom session worktrees, MCP filesystem tools like `mcp__filesystem__edit_file` require **relative paths** to work properly.

### The Problem
```bash
# This fails from session worktree:
mcp__filesystem__edit_file("/Users/rodk/github/flowloom/.github/scripts/sync-documentation.js")
# Error: undefined
```

### The Solution
```bash
# This works - use relative path from current directory:
mcp__filesystem__edit_file("../../.github/scripts/sync-documentation.js")
# Success!
```

## Why This Matters

1. **Security Boundaries**: MCP servers respect directory boundaries for security
2. **Session Isolation**: Session worktrees are isolated environments
3. **Path Resolution**: MCP resolves paths relative to the current working directory

## Practical Impact

When working in session directories like `/Users/rodk/github/flowloom/sessions/session-5189-website/`:
- Use `../../` to reach the main FlowLoom directory
- Use relative paths for all MCP filesystem operations
- This makes MCP tools "much more useful than bash shells and here-docs" (user quote)

## Related Quirks

### 1. Git Repository Corruption
- MCP git server was removed from FlowLoom due to corruption issues
- Always use standard bash git commands instead

### 2. Path Restrictions
- `cd` commands are restricted to child directories only
- Cannot cd to parent or sibling directories for security

### 3. Edit vs Bash for File Modifications
- MCP filesystem edit is preferred over bash heredocs
- More reliable, better error handling, and cleaner diffs

## Best Practices

1. **Always use relative paths** with MCP filesystem tools in session worktrees
2. **Test path access** before attempting edits
3. **Prefer MCP tools** over bash for file operations when possible
4. **Document path quirks** in project instructions (CLAUDE.md)

## Example: Fixing Regex Across Repository

Today's task demonstrated this perfectly - fixing regex errors in GitHub Actions scripts:
- Started in: `/Users/rodk/github/flowloom/sessions/session-5189-website/`
- Target file: `/Users/rodk/github/flowloom/.github/scripts/sync-documentation.js`
- Solution: Use `../../.github/scripts/sync-documentation.js`

This allowed seamless editing of main repository files from the session worktree!