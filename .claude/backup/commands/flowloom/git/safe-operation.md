Let input_args = "$ARGUMENTS"

# Safe Git Operation with Integrity Monitoring

Execute MCP git operations with automatic integrity checking to monitor for corruption.

## Argument Interpretation
First, analyze the provided arguments: input_args

Expected format: `<git_operation> [operation_args]`

Supported operations:
- `status` - Check repository status
- `add <files>` - Stage files for commit
- `diff [options]` - Show differences
- `log [options]` - Show commit history
- `reset [options]` - Reset changes
- `checkout <branch>` - Switch branches
- `show <commit>` - Show commit details

## Instructions

1. **Pre-operation integrity check** (optional baseline)
2. **Execute requested MCP git operation** using appropriate mcp__git__* function
3. **Post-operation integrity check** (mandatory)
4. **Log operation and results** for correlation tracking
5. **Report any integrity issues** immediately

## Operation Flow

### 1. Parse Operation
Extract git operation and arguments from input_args.

### 2. Execute MCP Git Operation
Use the appropriate MCP git server function:
- `status` → `mcp__git__git_status`
- `add` → `mcp__git__git_add`
- `diff` → `mcp__git__git_diff`, `mcp__git__git_diff_staged`, or `mcp__git__git_diff_unstaged`
- `log` → `mcp__git__git_log`
- `reset` → `mcp__git__git_reset`
- `checkout` → `mcp__git__git_checkout`
- `show` → `mcp__git__git_show`

### 3. Immediate Integrity Check
Run `/flowloom:system:git-integrity-check` immediately after the operation.

### 4. Results Logging
Log both the git operation results and integrity check results for pattern analysis.

## Output Format

```
🔧 Git Operation: <operation>
[git operation output]

🔍 Post-Operation Integrity Check:
[integrity check results]

📊 Operation Summary:
- Operation: <operation> [SUCCESS/FAILED]
- Integrity: [PASSED/WARNINGS/FAILED]
- Timestamp: <timestamp>
- Correlation ID: <unique_id>
```

## Usage Examples
- `safe-operation status` - Safe status check with integrity monitoring
- `safe-operation add file.txt` - Safe file staging with monitoring
- `safe-operation diff` - Safe diff with integrity verification

This wrapper ensures all git operations are monitored for corruption patterns and provides traceability for debugging git issues.