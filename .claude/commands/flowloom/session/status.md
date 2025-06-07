# Session Status Command

Check the status of your current FlowLoom session.

@bash
./bin/session_manager.sh status ${ARGUMENTS:-false}

Show the session status information above. If detailed status was requested, include comprehensive session information. If no session is found, suggest starting one with /flowloom:session:start.

## Options

**Basic Status (default):**
- Session ID, status, and uptime
- Working directory and git branch information
- Session context and last activity
- Command count and basic metadata

**Detailed Status:**
Use `detailed` as argument for comprehensive information:
- Current plan and active commands
- Recent files accessed
- Git state information
- Mode history and context history
- Todo items and memory entities
- Artifacts and session directory details

## Output Format

Returns JSON with session information:

```json
{
  "success": true,
  "hasSession": true,
  "sessionId": "session_123_456_abc123",
  "status": "active",
  "uptime": "2h 15m",
  "workingDirectory": "/path/to/worktree",
  "gitBranch": "session/abc123", 
  "sessionWorktree": "/Users/user/.flowloom/worktrees/session-abc123",
  "worktreeIsolation": true,
  "activeMode": "Default Mode",
  "sessionContext": "Working on session isolation",
  "commandCount": 42
}
```

## No Active Session

If no session is found:
```json
{
  "success": true,
  "hasSession": false,
  "message": "No active session found",
  "suggestion": "Start a new session with: /flowloom:session:start [context]"
}
```

## Notes

- Status is based on current PPID (process ID)
- Shows worktree isolation status if available
- Detailed mode provides comprehensive development context
- Use this to understand your current session state