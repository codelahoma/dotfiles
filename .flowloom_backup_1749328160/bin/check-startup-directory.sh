#!/bin/bash
# Check if Claude Code was started from a session directory
# This could cause memory.json corruption due to separate MCP server instances

# Get the current working directory where Claude was started
STARTUP_DIR="$(pwd)"

# Get the actual project root (handles worktrees correctly)
if git rev-parse --show-toplevel >/dev/null 2>&1; then
    # In a git worktree, get the main working tree
    PROJECT_ROOT="$(git worktree list | head -1 | awk '{print $1}')"
else
    # Fallback to parent directories
    PROJECT_ROOT="$(cd "$STARTUP_DIR/../.." && pwd)"
fi

# Check if we're in a session directory
if [[ "$STARTUP_DIR" =~ /sessions/session-[0-9]+-[a-zA-Z]+ ]]; then
    echo "⚠️  WARNING: Claude Code started from session directory!"
    echo ""
    echo "Starting Claude from a session directory will create a separate MCP memory server"
    echo "that writes to a local memory.json file, causing merge conflicts and data corruption."
    echo ""
    echo "Recommended workflow:"
    echo "1. Exit Claude Code"
    echo "2. cd to project root: cd $PROJECT_ROOT"
    echo "3. Start Claude from project root: claude"
    echo "4. Navigate to session within Claude: cd $STARTUP_DIR"
    echo ""
    echo "Current directory: $STARTUP_DIR"
    echo "Project root: $PROJECT_ROOT"
    exit 1
fi

exit 0