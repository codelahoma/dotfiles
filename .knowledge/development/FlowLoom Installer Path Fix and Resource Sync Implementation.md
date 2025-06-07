---
title: FlowLoom Installer Path Fix and Resource Sync Implementation
type: note
permalink: development/flow-loom-installer-path-fix-and-resource-sync-implementation
---

# FlowLoom Installer Path Fix and Resource Sync Implementation

## Issue Identified
The FlowLoom installer has a critical path issue where it writes `.claude` commands and `.mcp.json` to the FlowLoom config directory (`.flowloom/`) instead of the project root where Claude Code expects them.

### Root Cause
- **manager.py:350**: `target_claude = target_directory / ".claude"` writes to `.flowloom/.claude/`
- **generator.py:99**: `mcp_file = target_directory / ".mcp.json"` writes to `.flowloom/.mcp.json`
- Should use `target_directory.parent` to write to project root

## Solutions Implemented

### 1. CLI Terminology Clarification
Updated installer CLI with clearer terminology:
- **Project Root**: Where `.claude` and `.mcp.json` are created for Claude Code
- **FlowLoom Config Directory**: Where FlowLoom's installation files are stored (`.flowloom/`)

**New CLI Parameters:**
- `--project-root`: Project root directory (where Claude Code files go)
- `--flowloom-dir`: FlowLoom config directory (defaults to PROJECT_ROOT/.flowloom)
- Replaced `--target-dir` with `--flowloom-dir` for consistency

### 2. Resource Sync Command
Created `/flowloom:dev:update-installer-resources` command to sync project resources back to installer bundle:

**Syncs:**
- `.claude/` directory → installer bundled commands
- `.mcp.json` → MCP server config template
- `CLAUDE.local.md` → Claude config template
- `settings.local.json` → permission settings template

**Features:**
- Dry run mode (`--dry-run`)
- Detailed logging and statistics
- Safety checks and validation
- Clear next steps after syncing

### 3. Project Root Cleanup Planning
Identified files for cleanup in project root:

**To Delete (temporary/artifacts):**
- Test directories: `flowloom-home-test-92599/`, `test-*-project/`
- Git analysis artifacts: `git_analysis_*/`, `git_project_analysis_*.txt`
- Backup files: `*.backup-*`
- Temporary files: `session.log`, `tmp/commit-msg.txt`

**To Move to `/docs` (reports/documentation):**
- Various implementation reports and analysis documents
- Research documents and compatibility analyses

**To Move to `/tests`:**
- `test_ppa_sessions.py`
- `validation-tests/`

### 4. Settings File Consolidation
Discovered duplicate `settings.local.json` files:
- Project root version (5.2k, older) had 148 permissions
- `.claude/settings.local.json` (9.7k, newer) had 185 permissions
- Successfully merged missing permissions from root to `.claude` version
- Final merged file has 201 permissions
- Root version can now be safely deleted

## Next Steps
1. **Fix installer paths** - Update manager.py and generator.py to use project root
2. **Execute cleanup plan** - Organize project root files
3. **Test installer fixes** - Verify `.claude` and `.mcp.json` go to correct locations
4. **Update installer resources** - Run sync command to bundle latest configs

## Files Modified
- `/packages/flowloom_installer/src/flowloom_installer/cli.py` - Updated terminology and parameters
- `/.claude/settings.local.json` - Merged permissions from root settings
- `/.claude/commands/flowloom/dev/update-installer-resources.md` - New sync command

## Status
- ✅ CLI terminology improved
- ✅ Resource sync command created
- ✅ Settings files consolidated
- ✅ Cleanup plan prepared
- ⏳ Installer path fix (pending)
- ⏳ Cleanup execution (pending)