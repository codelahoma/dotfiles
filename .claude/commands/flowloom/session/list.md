# List Sessions Command

List all FlowLoom sessions using the Python implementation.

Let input_args = "$ARGUMENTS"

## Argument Interpretation
- If input_args is empty: List active sessions for current shell
- If input_args is "all": List all sessions across all shells
- If input_args is "inactive": Include inactive sessions

@bash
# Parse arguments
ARGS=""
if [ "$input_args" = "all" ]; then
    # For now, just show current shell sessions
    # Future: implement cross-shell listing
    ARGS=""
elif [ "$input_args" = "inactive" ]; then
    ARGS="--inactive"
fi

./bin/flowloom-session list $ARGS

Show the session list above. Each session shows its ID, purpose, status, and creation time.

## Session List Features

**Shell PID Isolation:**
- By default, only shows sessions for current shell
- Each Claude instance has isolated sessions
- Prevents confusion between different work contexts

**Session Information:**
- Session ID format: `{shell_pid}-{purpose}`
- Status: active, inactive, completed, abandoned
- Creation time and last activity
- Purpose/description

## List Command Options

- `list` - Active sessions for current shell (default)
- `list inactive` - Include inactive/completed sessions
- `list all` - All sessions (future feature)

## Example Output

```
Found 2 active sessions:

📋 23310-feature-development
   Purpose: feature-development
   Status: active
   Created: 2025-05-29 10:30:00
   Shell PID: 23310

📋 23310-bug-fix
   Purpose: bug-fix
   Status: active
   Created: 2025-05-29 11:45:00
   Shell PID: 23310
```

## Session Management

After listing sessions, you can:
- `flowloom-session status` - Get detailed status
- `flowloom-session switch {purpose}` - Switch to session
- `flowloom-session commit {id}` - Commit changes
- `flowloom-session cleanup` - Remove old sessions

## Notes

- PPA approach provides instant listing
- No git operations required
- Sessions persist until explicit cleanup
- Shell isolation ensures clean workspace