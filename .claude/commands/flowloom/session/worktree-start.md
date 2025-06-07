Let input_args = "$ARGUMENTS"

# Worktree Session Start Command

Start a new FlowLoom session with git worktree isolation and branch separation.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Start session with generic context
- If input_args contains text: Use as session context description

## Implementation

Create an isolated session with worktree and branch:

@bash
echo "🚀 Starting Worktree Session..."
echo "==============================="

# Generate session ID
SESSION_ID="worktree-$(date +%Y%m%d-%H%M%S)-$$"
SHORT_ID="${SESSION_ID: -12}"
SESSION_BRANCH="session/$SHORT_ID"
CONTEXT="${input_args:-Worktree session started}"

echo "Session ID: $SESSION_ID"
echo "Branch: $SESSION_BRANCH"
echo "Context: $CONTEXT"

# Check git repository
if ! git rev-parse --git-dir >/dev/null 2>&1; then
    echo "❌ Error: Not in a git repository"
    echo "Worktree sessions require a git repository"
    exit 1
fi

# Set up worktree directory - trust FLOWLOOM_WORK_DIR when present
if [[ -n "$FLOWLOOM_WORK_DIR" ]]; then
    WORK_BASE_DIR="$FLOWLOOM_WORK_DIR/worktrees"
else
    WORK_BASE_DIR="$HOME/.flowloom/worktrees"
fi
WORKTREE_DIR="$WORK_BASE_DIR/session-$SHORT_ID"

echo ""
echo "🏗️  Creating worktree isolation..."
echo "Base directory: $WORK_BASE_DIR"
echo "Worktree path: $WORKTREE_DIR"

# Create base directory
if ! mkdir -p "$WORK_BASE_DIR"; then
    echo "❌ Error: Failed to create worktree base directory"
    exit 1
fi

# Create worktree with new branch
echo ""
echo "📝 Creating session branch and worktree..."
if git worktree add "$WORKTREE_DIR" -b "$SESSION_BRANCH" 2>/dev/null; then
    echo "✅ Session worktree created: $WORKTREE_DIR"
    echo "✅ Session branch created: $SESSION_BRANCH"
    
    # Create session metadata in the worktree
    SESSION_DIR="$WORKTREE_DIR/.flowloom/sessions/$SESSION_ID"
    mkdir -p "$SESSION_DIR"
    
    # Create session metadata using Python
    python3 -c "
import json
import datetime
import subprocess
import os
import sys

try:
    session_data = {
        'sessionId': '$SESSION_ID',
        'context': '$CONTEXT',
        'startTime': datetime.datetime.utcnow().isoformat() + 'Z',
        'workingDirectory': '$WORKTREE_DIR',
        'gitBranch': '$SESSION_BRANCH',
        'worktreeIsolation': True,
        'sessionType': 'worktree',
        'originalDirectory': os.getcwd(),
        'worktreeDirectory': '$WORKTREE_DIR'
    }
    
    # Write session file
    with open('$SESSION_DIR/session.json', 'w') as f:
        json.dump(session_data, f, indent=2)
    
    print('')
    print('📊 Session Details:')
    print(json.dumps(session_data, indent=2))
    
except Exception as e:
    print(f'❌ Error creating session metadata: {e}', file=sys.stderr)
    sys.exit(1)
"
    
    # Store current session pointer in worktree
    echo "$SESSION_ID" > "$WORKTREE_DIR/.flowloom/current-session"
    
    # Register session in PPID registry for auto-resume
    CURRENT_PPID="${PPID:-$$}"
    PPID_REGISTRY=".flowloom/ppid-registry.json"
    
    python3 -c "
import json
import os

registry_file = '$PPID_REGISTRY'
session_data = {
    'sessionId': '$SESSION_ID',
    'worktreeDirectory': '$WORKTREE_DIR',
    'created': '$(date -u +%Y-%m-%dT%H:%M:%SZ)',
    'lastAccess': '$(date -u +%Y-%m-%dT%H:%M:%SZ)'
}

try:
    if os.path.exists(registry_file):
        with open(registry_file, 'r') as f:
            registry = json.load(f)
    else:
        registry = {}
    
    if '$CURRENT_PPID' not in registry:
        registry['$CURRENT_PPID'] = {}
    
    registry[\"$CURRENT_PPID\"][\"$SESSION_ID\"] = session_data
    
    os.makedirs(os.path.dirname(registry_file), exist_ok=True)
    with open(registry_file, 'w') as f:
        json.dump(registry, f, indent=2)
    
    print('✅ Session registered for auto-resume (PPID: $CURRENT_PPID)')
        
except Exception as e:
    print(f'⚠️  Warning: Could not register session for auto-resume: {e}')
"
    
    echo ""
    echo "🎯 Next Steps:"
    echo "1. cd $WORKTREE_DIR"
    echo "2. Work in complete isolation"
    echo "3. Use /flowloom:session:worktree-stop when finished"
    echo ""
    echo "💡 Your changes are isolated in branch: $SESSION_BRANCH"
    
else
    echo "❌ Error: Failed to create worktree"
    echo "This might happen if:"
    echo "  - Branch name already exists"
    echo "  - Insufficient permissions"
    echo "  - Git repository issues"
    exit 1
fi

Show the user the session information above including the worktree path and instructions for switching to the isolated environment.

## Worktree Session Features

**Complete Isolation:**
- Separate working directory for all files
- Independent git branch for commits
- No interference with main development
- Parallel session support

**Session Management:**
- Session metadata stored in worktree
- Automatic cleanup when session ends
- Recovery support for interrupted sessions
- Integration with FlowLoom memory system

**Git Integration:**
- Session-specific branch creation
- Automatic worktree management
- Safe cleanup without affecting main repo
- Commit isolation until merge

## Related Commands
- `/flowloom:session:worktree-stop` - End worktree session
- `/flowloom:session:status` - Check session status
- `/flowloom:session:list` - List all sessions