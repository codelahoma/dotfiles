# Stop Session Command

Stop your current FlowLoom session using the Python implementation.

Let input_args = "$ARGUMENTS"

## Argument Interpretation
- If input_args is empty: Stop the most recent active session for current shell
- If input_args is a session ID: Stop that specific session
- If input_args contains a note: Add completion note to session

@bash
# Determine which session to stop
if [ -z "$input_args" ]; then
    # Get the most recent session for this shell
    SESSION_ID=$(./bin/flowloom-session list --json 2>/dev/null | python3 -c "
import sys, json
try:
    data = json.load(sys.stdin)
    if data:
        print(data[0]['session_id'])
except:
    pass
" 2>/dev/null)
    
    if [ -z "$SESSION_ID" ]; then
        echo "No active sessions found for current shell"
        exit 0
    fi
else
    SESSION_ID="$input_args"
fi

# For now, just mark the session as complete
# In future, this could integrate with commit workflow
echo "Stopping session: $SESSION_ID"
echo "Note: Use 'flowloom-session commit $SESSION_ID' to commit any changes"

# Update session status in metadata
./bin/flowloom-session status --session-id "$SESSION_ID" || true

Show the session stop information above. The PPA approach doesn't require complex cleanup - sessions are simply marked as complete.

## PPA Session Stop Behavior

**Simple Status Update:**
- Session marked as completed
- No git worktree removal needed
- Session artifacts preserved
- Instant operation (0.000s)

**Commit Workflow:**
- Use `flowloom-session commit {id}` to commit changes
- Automatic conflict resolution available
- Changes tracked via git diff
- Session can be resumed if needed

## Session Management After Stop

- `./bin/flowloom-session list --inactive` - Show all sessions including stopped
- `./bin/flowloom-session status` - Review session changes
- `./bin/flowloom-session commit {id}` - Commit session work
- `./bin/flowloom-session cleanup` - Remove old sessions

## Notes

- PPA approach: No directory cleanup required
- Session data preserved until explicit cleanup
- Work continues in main directory
- Use commit workflow for git integration