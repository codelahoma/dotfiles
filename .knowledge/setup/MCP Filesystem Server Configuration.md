---
title: MCP Filesystem Server Configuration
type: note
permalink: setup/mcp-filesystem-server-configuration
tags:
- '#mcp'
- '#filesystem'
- '#configuration'
- '#permissions'
---

# MCP Filesystem Server Configuration

## Critical Requirements

The MCP filesystem server has specific requirements that must be understood for proper operation:

### Allowed Directories Configuration

The filesystem server **REQUIRES** explicit directory permissions in your MCP configuration. Without proper `allowedDirectories`, all file operations will fail silently.

**Required Configuration in `.mcp.json`:**
```json
{
  "mcpServers": {
    "filesystem": {
      "command": ["npx", "-y", "@modelcontextprotocol/server-filesystem"],
      "args": ["--allowed-directories", "/path/to/your/project"],
      "enabled": true
    }
  }
}
```

### Path Requirements

1. **Relative Paths Only**: The filesystem server works with paths relative to the allowed directories
   - ✅ Correct: `src/components/Button.tsx`
   - ❌ Wrong: `/Users/name/project/src/components/Button.tsx`

2. **Current Directory Context**: The server operates relative to where Claude Code was launched
   - Always launch Claude Code from your project root
   - Use `pwd` to verify your working directory

3. **Multiple Directories**: You can allow multiple directories:
   ```json
   "args": ["--allowed-directories", "/path/one,/path/two,/path/three"]
   ```

## Common Issues and Solutions

### "Permission Denied" Errors
**Symptom**: File operations fail with permission errors
**Cause**: Directory not in allowedDirectories list
**Solution**: Add the directory to your MCP configuration

### "File Not Found" Errors
**Symptom**: Known files can't be accessed
**Cause**: Using absolute paths instead of relative
**Solution**: Convert to relative paths from the allowed directory

### Silent Failures
**Symptom**: File operations appear to work but nothing happens
**Cause**: MCP server not properly configured
**Solution**: Check `claude mcp list` to verify server is running

## Best Practices

1. **Set Project Root**: Always set your project root as an allowed directory
2. **Use Relative Paths**: Train yourself to think in relative paths
3. **Verify Configuration**: Run `claude mcp list` after configuration changes
4. **Multiple Projects**: Use comma-separated paths for multiple projects

## FlowLoom Integration

FlowLoom automatically configures the filesystem server during installation:
- Sets the project directory as allowed
- Configures relative path handling
- Provides helper commands that use correct paths

## Security Notes

The allowedDirectories restriction is a security feature:
- Prevents accidental access to system files
- Limits operations to explicit project boundaries
- Protects sensitive directories outside your project

Remember: This is a feature, not a limitation. It keeps your system safe while providing necessary access.