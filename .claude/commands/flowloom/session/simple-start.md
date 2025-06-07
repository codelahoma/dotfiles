Start new FlowLoom session with basic tracking (no worktree isolation)
Let input_args = "$ARGUMENTS"

# Simple Session Start Command

Start a new FlowLoom session with basic tracking (no worktree isolation).

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Start session with generic context
- If input_args contains text: Use as session context description

## Implementation

Create a simple session without worktree complexity:

@bash
# Create session directory structure
SESSION_ID="simple-$(date +%Y%m%d-%H%M%S)-$$"
SESSION_DIR=".flowloom/sessions/$SESSION_ID"
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
        'context': '${input_args:-Simple session started}',
        'startTime': datetime.datetime.utcnow().isoformat() + 'Z',
        'workingDirectory': os.getcwd(),
        'gitBranch': 'no-git',
        'worktreeIsolation': False,
        'sessionType': 'simple'
    }
    
    # Try to get git branch
    try:
        result = subprocess.run(['git', 'branch', '--show-current'], 
                              capture_output=True, text=True, check=True)
        session_data['gitBranch'] = result.stdout.strip() or 'main'
    except:
        pass
    
    # Write session file
    with open('$SESSION_DIR/session.json', 'w') as f:
        json.dump(session_data, f, indent=2)
    
    print('✅ Session started successfully!')
    print('')
    print('Session Details:')
    print(json.dumps(session_data, indent=2))
    
except Exception as e:
    print(f'❌ Error creating session: {e}', file=sys.stderr)
    sys.exit(1)
"

# Store current session
echo "$SESSION_ID" > ".flowloom/current-session"

Show the user the session information above. This creates a basic session tracking system without worktree complexity.

## Session Features

**Basic Tracking:**
- Session metadata in `.flowloom/sessions/[session-id]/`
- Current session tracking in `.flowloom/current-session`
- JSON-based session information
- Git branch tracking (if available)

**No Isolation:**
- Works in current directory
- No worktree creation
- Simpler for quick development sessions
- Compatible with existing workflow

## Related Commands
- `/flowloom:session:status` - Check current session
- `/flowloom:session:stop` - End current session
- `/flowloom:session:list` - List all sessions