Let input_args = "$ARGUMENTS"

# Configuration Utilities

Advanced configuration management utilities for FlowLoom system administration.

## What This Command Does

1. **Configuration Backup and Restore**
   - Create snapshots of current configuration state
   - Restore previous configuration versions
   - Manage configuration versioning

2. **Settings Validation and Repair**
   - Validate JSON syntax in all configuration files
   - Repair common configuration issues automatically
   - Merge configuration updates safely

3. **Permission Management**
   - Analyze current permission settings
   - Suggest optimal permission configurations
   - Validate tool access and capabilities

4. **Environment Variable Management**
   - Check required environment variables
   - Suggest missing configurations
   - Validate environment setup

## Argument Interpretation

First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Show available utilities
- If input_args is "backup": Create configuration backup
- If input_args is "restore": Restore from backup
- If input_args is "validate": Validate all configurations
- If input_args is "repair": Auto-repair configuration issues
- If input_args is "permissions": Analyze permission settings
- If input_args is "env": Check environment variables

## Argument Patterns
- (no arguments) - Show available configuration utilities
- `backup` - Create timestamped configuration backup
- `restore [timestamp]` - Restore configuration from backup
- `validate` - Validate all configuration files
- `repair` - Automatically fix common issues
- `permissions` - Analyze and optimize permissions
- `env` - Check environment variable setup

## Implementation

Provide advanced configuration management:

### Configuration Backup
- Create timestamped snapshots of key files
- Include CLAUDE.md, settings, .mcp.json, command structure
- Store in .claude/backups/ with rotation policy
- Provide restore capabilities

### Validation and Repair
- Parse JSON files for syntax errors
- Check for missing required fields
- Validate file permissions and access
- Auto-fix common configuration problems

### Permission Analysis
- Review current tool allowlists
- Suggest optimizations for security and functionality
- Identify unused or redundant permissions
- Recommend permission consolidation

### Environment Setup
- Check for required environment variables
- Validate API keys and tokens
- Suggest missing configuration
- Test environment variable accessibility

## Output Format

Provide clear utility interface:

```
FlowLoom Configuration Utilities 🔧

Available Utilities:
1. backup    - Create configuration snapshot
2. restore   - Restore from previous backup
3. validate  - Check configuration validity
4. repair    - Auto-fix common issues
5. permissions - Analyze permission settings
6. env       - Environment variable check

Usage: system:config-utils [utility] [options]

Recent Backups:
- 2025-05-29_19-30-15 (before system commands)
- 2025-05-29_15-20-45 (before uv migration)
- 2025-05-28_14-10-30 (before session management)

Quick Actions:
- system:config-utils backup
- system:config-utils validate
- system:config-utils env
```

### Backup Operation
```
Configuration Backup Created ✅

Backup ID: 2025-05-29_19-35-22
Location: .claude/backups/config-backup-2025-05-29_19-35-22/

Files backed up:
- CLAUDE.local.md (13.7KB)
- .claude/settings.local.json (8.2KB)
- .mcp.json (1.1KB)
- .claude/commands/ (122 files)

Restore command:
system:config-utils restore 2025-05-29_19-35-22
```

### Validation Results
```
Configuration Validation Report 📋

✅ CLAUDE.local.md: Valid (no issues)
✅ .claude/settings.local.json: Valid JSON
✅ .mcp.json: Valid configuration
⚠️ Missing environment variable: GITHUB_TOKEN
✅ Command structure: 122 commands validated

Issues Found: 1
- Set GITHUB_TOKEN for full GitHub functionality

All critical configurations valid.
```

### Permission Analysis
```
Permission Analysis Report 🔐

Current Permissions: 78 allowed tools
Security Level: Standard (appropriate for development)

Recommendations:
✅ MCP servers properly configured
✅ File operations appropriately scoped
⚠️ Consider restricting bash wildcard permissions
✅ GitHub operations properly limited

Optimization Opportunities:
- Consolidate similar bash permissions
- Review unused tool permissions
- Consider permission grouping
```

## Advanced Features

### Automated Repair
- Fix JSON syntax errors where possible
- Restore missing required configuration fields
- Repair broken file paths and references
- Update deprecated configuration patterns

### Configuration Diff
- Compare current configuration with backups
- Show what changed between versions
- Highlight potentially problematic changes
- Suggest rollback for specific changes

### Environment Optimization
- Detect optimal environment variable setup
- Suggest configuration improvements
- Validate API key functionality
- Test service connectivity

### Security Audit
- Review permission configurations for security
- Identify overly broad tool access
- Suggest principle of least privilege improvements
- Check for sensitive data exposure

This utility provides comprehensive configuration management for maintaining optimal FlowLoom system health and security.