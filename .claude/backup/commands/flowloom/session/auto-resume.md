Let input_args = "$ARGUMENTS"

# Auto-Resume Session Command

Automatically discover and resume existing sessions based on PPID.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Auto-discover and resume session for current PPID
- If input_args contains "list": Show all resumable sessions
- If input_args contains session ID: Resume specific session

## Implementation

@bash
echo "🔍 Auto-Resume Session Discovery"
echo "================================="

# Get current PPID for session discovery
CURRENT_PPID="${PPID:-$$}"
echo "Current PPID: $CURRENT_PPID"

# Check for PPID-based session registry
PPID_REGISTRY=".flowloom/ppid-registry.json"

if [[ "$input_args" == "list" ]]; then
    echo ""
    echo "📋 All Resumable Sessions:"
    if [[ -f "$PPID_REGISTRY" ]]; then
        python3 -c "
import json
try:
    with open('$PPID_REGISTRY', 'r') as f:
        registry = json.load(f)
    
    print('PPID → Session ID → Worktree Status')
    print('-' * 50)
    for ppid, sessions in registry.items():
        for session_id, data in sessions.items():
            worktree_dir = data.get('worktreeDirectory', 'unknown')
            status = 'ACTIVE' if os.path.exists(worktree_dir) else 'MISSING'
            print(f'{ppid} → {session_id} → {status}')
            
except FileNotFoundError:
    print('No session registry found')
except Exception as e:
    print(f'Error reading registry: {e}')
"
    else
        echo "No session registry found"
    fi
    exit 0
fi

# Discover existing sessions for current PPID
echo ""
echo "🔍 Searching for existing sessions..."

FOUND_SESSION=""
WORKTREE_DIR=""

if [[ -f "$PPID_REGISTRY" ]]; then
    # Check PPID registry for existing sessions
    REGISTRY_RESULT=$(python3 -c "
import json
import os
try:
    with open('$PPID_REGISTRY', 'r') as f:
        registry = json.load(f)
    
    ppid_sessions = registry.get('$CURRENT_PPID', {})
    for session_id, data in ppid_sessions.items():
        worktree_dir = data.get('worktreeDirectory', '')
        if worktree_dir and os.path.exists(worktree_dir):
            print(f'{session_id}|{worktree_dir}')
            break
    
except:
    pass
")
    
    if [[ -n "$REGISTRY_RESULT" ]]; then
        FOUND_SESSION="${REGISTRY_RESULT%|*}"
        WORKTREE_DIR="${REGISTRY_RESULT#*|}"
        echo "✅ Found existing session: $FOUND_SESSION"
        echo "📁 Worktree directory: $WORKTREE_DIR"
    fi
fi

# Fallback: Search for active worktrees
if [[ -z "$FOUND_SESSION" ]]; then
    echo "🔍 Searching active worktrees..."
    
    # Look for worktrees with current-session files
    WORK_BASE_DIR="${FLOWLOOM_WORK_DIR:-$HOME/.flowloom}/worktrees"
    
    if [[ -d "$WORK_BASE_DIR" ]]; then
        for worktree in "$WORK_BASE_DIR"/session-*; do
            if [[ -d "$worktree" && -f "$worktree/.flowloom/current-session" ]]; then
                SESSION_ID=$(cat "$worktree/.flowloom/current-session" 2>/dev/null)
                if [[ -n "$SESSION_ID" ]]; then
                    FOUND_SESSION="$SESSION_ID"
                    WORKTREE_DIR="$worktree"
                    echo "✅ Found worktree session: $FOUND_SESSION"
                    echo "📁 Worktree directory: $WORKTREE_DIR"
                    break
                fi
            fi
        done
    fi
fi

# Resume session if found
if [[ -n "$FOUND_SESSION" && -n "$WORKTREE_DIR" ]]; then
    echo ""
    echo "🚀 Resuming Session: $FOUND_SESSION"
    echo "==================================="
    
    # Display session info
    SESSION_FILE="$WORKTREE_DIR/.flowloom/sessions/$FOUND_SESSION/session.json"
    if [[ -f "$SESSION_FILE" ]]; then
        echo "📊 Session Details:"
        python3 -c "
import json
try:
    with open('$SESSION_FILE', 'r') as f:
        data = json.load(f)
    print(json.dumps(data, indent=2))
except Exception as e:
    print(f'Error reading session: {e}')
"
    fi
    
    # Register current PPID if not already registered
    python3 -c "
import json
import os

registry_file = '$PPID_REGISTRY'
session_data = {
    'sessionId': '$FOUND_SESSION',
    'worktreeDirectory': '$WORKTREE_DIR',
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
    
    registry['$CURRENT_PPID']['$FOUND_SESSION'] = session_data
    
    os.makedirs(os.path.dirname(registry_file), exist_ok=True)
    with open(registry_file, 'w') as f:
        json.dump(registry, f, indent=2)
    
    print('✅ PPID registry updated')
        
except Exception as e:
    print(f'⚠️  Warning: Could not update PPID registry: {e}')
"
    
    echo ""
    echo "🎯 Next Steps:"
    echo "cd $WORKTREE_DIR"
    echo ""
    echo "💡 You are now ready to continue working in your isolated session!"
    
else
    echo ""
    echo "ℹ️  No existing sessions found for PPID $CURRENT_PPID"
    echo ""
    echo "🚀 Start a new session with:"
    echo "   /flowloom:session:worktree-start [context]"
fi

Show the user the session discovery results and next steps for resuming or starting a new session.

## PPID Session Registry

**Registry File:** `.flowloom/ppid-registry.json`
**Purpose:** Maps PPID → Session ID → Worktree Directory
**Benefits:** Fast session discovery, automatic resume capability

**Registry Structure:**
```json
{
  "12345": {
    "session-abc123": {
      "sessionId": "session-abc123",
      "worktreeDirectory": "./worktrees/session-abc123",
      "lastAccess": "2025-05-26T13:45:00Z"
    }
  }
}
```

## Auto-Resume Features

**Session Discovery:**
- PPID-based session lookup
- Active worktree scanning
- Session metadata validation
- Registry maintenance

**Automatic Resume:**
- Session state restoration
- Worktree directory identification
- Last access time tracking
- Cross-session compatibility

## Related Commands
- `/flowloom:session:worktree-start` - Start new worktree session
- `/flowloom:session:status` - Check current session status
- `/flowloom:session:auto-resume list` - List all resumable sessions