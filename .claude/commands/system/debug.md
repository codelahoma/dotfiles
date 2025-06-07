Let input_args = "$ARGUMENTS"

# System Debug and Troubleshooting

Comprehensive debugging tools for FlowLoom system issues.

## What This Command Does

1. **Performance Diagnostics**
   - Check system resource usage and bottlenecks
   - Measure command execution times
   - Analyze memory and disk usage patterns

2. **Configuration Validation**
   - Verify all configuration files are syntactically valid
   - Check for missing required files or permissions
   - Validate environment variable settings

3. **Connectivity Testing**
   - Test MCP server connections and response times
   - Verify file system access and permissions
   - Check network connectivity for remote services

4. **Error Log Analysis**
   - Scan for recent errors in logs and outputs
   - Identify common failure patterns
   - Suggest specific remediation steps

## Argument Interpretation

First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Full system debug analysis
- If input_args is "performance": Focus on performance diagnostics
- If input_args is "config": Configuration validation only
- If input_args is "connectivity": Network and server testing
- If input_args is "logs": Error log analysis
- If input_args is "memory": Memory system diagnostics
- If input_args is "quick": Fast essential checks only

## Argument Patterns
- (no arguments) - Complete diagnostic suite
- `performance` - System performance analysis
- `config` - Configuration file validation
- `connectivity` - Server and network testing
- `logs` - Error analysis and troubleshooting
- `memory` - Memory graph and knowledge system diagnostics
- `quick` - Essential health checks only

## Implementation

Perform comprehensive system diagnostics:

### Performance Diagnostics
- Measure command execution times
- Check available disk space and memory
- Test file operation performance
- Identify potential bottlenecks

### Configuration Validation
- Parse and validate JSON configuration files
- Check for missing environment variables
- Verify file permissions and access
- Validate command structure and syntax

### Connectivity Testing
- Test each MCP server connection
- Measure server response times
- Check file system access patterns
- Verify network connectivity

### Error Log Analysis
- Scan recent command outputs for errors
- Check system logs for failure patterns
- Identify common configuration issues
- Analyze performance degradation patterns

### Memory System Diagnostics
- Use Memory Monitor for graph statistics
- Check knowledge base integrity
- Analyze memory usage patterns
- Verify data consistency

## Output Format

Provide detailed diagnostic results:

```
FlowLoom System Debug Report 🔍

⚡ Performance Analysis
- Command Response: ✅ Avg 0.15s (Good)
- Disk Space: ✅ 45GB free (Healthy)
- Memory Usage: ✅ 2.1GB available (Normal)
- File Operations: ✅ <50ms avg (Fast)

🔧 Configuration Status
- CLAUDE.local.md: ✅ Valid syntax
- .mcp.json: ✅ All servers configured
- Settings: ✅ 78 permissions active
- Environment: ⚠️ GITHUB_TOKEN not set

🔌 Connectivity Tests
- Memory Server: ✅ 45ms response
- Filesystem: ✅ 12ms response
- GitHub API: ❌ Authentication failed
- SQLite: ✅ 8ms query time

📊 Memory Diagnostics
- Graph Entities: 156 (use Memory Monitor)
- Knowledge Docs: 59 active
- SQLite Records: 6 development notes
- Watch Service: ✅ Active (PID: 63391)

🚨 Issues Found
1. GitHub token not configured
   → Set GITHUB_TOKEN environment variable
2. Large memory graph (>75k tokens)
   → Use uv run flowmon for queries

💡 Recommendations
- Configure GitHub authentication
- Use Memory Monitor for large graph queries
- Consider cleanup of old session data
```

## Troubleshooting Guides

Provide specific solutions for common issues:

### Configuration Issues
- Invalid JSON syntax fixes
- Missing environment variable setup
- Permission problems and solutions

### Performance Issues
- Disk space cleanup recommendations
- Memory optimization strategies
- Command execution optimization

### Connectivity Problems
- MCP server restart procedures
- Network troubleshooting steps
- Authentication fix guidance

### Memory System Issues
- Graph cleanup strategies
- Knowledge base maintenance
- Data consistency verification

## Recovery Actions

When critical issues are detected:
- Automatic backup recommendations
- Safe recovery procedures
- Rollback strategies for configuration changes
- Emergency contact procedures for data loss

This command is essential for maintaining FlowLoom system health and quickly diagnosing issues.