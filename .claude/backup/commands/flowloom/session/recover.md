# Recover Session Command

Recover and restore a previous FlowLoom session.

## Context

You want to resume work from a previous session. This will:
- Restore session state and metadata
- Recreate git worktree if needed
- Switch to session branch and directory
- Restore todo items and context
- Continue where you left off

## Task

Execute the session manager to recover a session:

```bash
./bin/session_manager.sh recover ${ARGUMENTS:-}
```

## Recovery Options

**Auto-Recovery (no arguments):**
- Finds most recent session for current PPID
- Automatically restores latest session

**Specific Session ID:**
- Recover exact session: `recover session_123_456_abc123`
- Must provide full session ID

**Recovery Modes:**
- `full`: Complete state restoration (default)
- `state-only`: Restore state without files
- `metadata-only`: Basic session info only

## Recovery Actions

**Worktree Recovery:**
- `worktree_restored`: Existing worktree found and entered
- `worktree_recreated`: Worktree recreated from existing branch
- `worktree_recreate_failed`: Could not recreate worktree
- `worktree_and_branch_not_found`: Session completely gone

**Legacy Branch Recovery:**
- `branch_restored`: Session branch checked out
- `branch_not_found`: Session branch no longer exists
- `branch_checkout_failed`: Could not switch to branch

## Session Restoration

The recovery process:
1. Validates session ID exists
2. Creates backup of current work
3. Restores session from backup
4. Handles git worktree/branch recovery
5. Updates session as active
6. Changes to session working directory

## Example Output

```json
{
  "success": true,
  "sessionId": "session_123_456_abc123",
  "recoveryMode": "full",
  "sessionContext": "Working on session isolation",
  "workingDirectory": "/Users/user/.flowloom/worktrees/session-abc123",
  "sessionWorktree": "/Users/user/.flowloom/worktrees/session-abc123",
  "worktreeIsolation": true,
  "recoveryAction": "worktree_restored",
  "recoveredAt": "2025-01-26T14:30:00",
  "sessionDirectory": "/path/to/session/data"
}
```

## Recovery Failure

If recovery fails:
- Session ID not found
- Invalid session format
- Backup restoration failed
- Git worktree issues

## Notes

- Auto-recovery uses current PPID to find sessions
- Worktree recreation preserves git history
- Recovery updates session as active
- Previous work context is restored
- Use `/flowloom:session:list` to find available sessions