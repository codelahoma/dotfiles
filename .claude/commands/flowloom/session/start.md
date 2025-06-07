# Start Session Command

Start a new FlowLoom session using the Python implementation with directory-based isolation.

@bash
./bin/flowloom-session create "$ARGUMENTS"

Show the session information above. If session creation fails, show the error and suggest checking the FlowLoom installation.

## Session Isolation Features (PPA - Python Preferred Approach)

**Directory-Based Isolation:**
- Sessions tracked in `sessions/{shell_pid}-{purpose}/`
- Work remains in main project directory
- Session artifacts stored separately
- No git worktree complexity

**Performance Benefits:**
- 0.093s session lifecycle
- Instant stop/cleanup operations
- No branch switching overhead
- Simplified conflict resolution

**State Tracking:**
- Session metadata and state persistence
- Memory integration and todo tracking
- Git diff-based change tracking
- Automatic conflict detection

## After Session Start

The session will be created with:
- `Session ID`: {shell_pid}-{purpose} format
- `Working Directory`: Always main project directory
- `Session Directory`: `sessions/{session_id}/` for artifacts
- `Status`: Active session ready for work

Continue working in your main project directory - all changes are tracked by session.

## Session Management Commands

- `./bin/flowloom-session list` - List all sessions
- `./bin/flowloom-session status` - Check session status
- `./bin/flowloom-session commit {id}` - Commit session changes
- `./bin/flowloom-session cleanup` - Clean up old sessions

## Notes

- PPA approach: No directory switching required
- Sessions are shell PID isolated
- All session data preserved in session directory
- Use `/flowloom:session:stop` or `flowloom-session commit` when finished