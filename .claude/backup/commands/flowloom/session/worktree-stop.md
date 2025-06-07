Let input_args = "$ARGUMENTS"

# Worktree Session Stop Command

End a worktree session and clean up the isolated environment.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Stop current session
- If input_args contains session ID: Stop specific session
- If input_args contains "force": Force cleanup even with uncommitted changes

## Implementation

@bash
echo "🛑 Stopping Worktree Session..."
echo "==============================="

# Check if we're in a worktree session
CURRENT_DIR="$(pwd)"
FORCE_STOP=false

if [[ "$input_args" == *"force"* ]]; then
    FORCE_STOP=true
    echo "⚠️  Force stop enabled - will cleanup uncommitted changes"
fi

# Check for current session
if [[ -f ".flowloom/current-session" ]]; then
    CURRENT_SESSION=$(cat .flowloom/current-session)
    SESSION_DIR=".flowloom/sessions/$CURRENT_SESSION"
    
    echo "Found session: $CURRENT_SESSION"
    
    if [[ -f "$SESSION_DIR/session.json" ]]; then
        # Get session info
        WORKTREE_DIR=$(python3 -c "
import json
try:
    with open('$SESSION_DIR/session.json', 'r') as f:
        data = json.load(f)
    print(data.get('worktreeDirectory', ''))
except:
    pass
")
        
        ORIGINAL_DIR=$(python3 -c "
import json
try:
    with open('$SESSION_DIR/session.json', 'r') as f:
        data = json.load(f)
    print(data.get('originalDirectory', ''))
except:
    pass
")
        
        SESSION_BRANCH=$(python3 -c "
import json
try:
    with open('$SESSION_DIR/session.json', 'r') as f:
        data = json.load(f)
    print(data.get('gitBranch', ''))
except:
    pass
")
        
        echo "Worktree: $WORKTREE_DIR"
        echo "Original: $ORIGINAL_DIR" 
        echo "Branch: $SESSION_BRANCH"
        
        # Check for uncommitted changes
        if [[ -n "$SESSION_BRANCH" ]] && [[ "$FORCE_STOP" == "false" ]]; then
            UNCOMMITTED=$(git status --porcelain 2>/dev/null | wc -l | xargs)
            if [[ "$UNCOMMITTED" -gt 0 ]]; then
                echo ""
                echo "⚠️  Warning: $UNCOMMITTED uncommitted changes detected"
                echo "Files with changes:"
                git status --porcelain 2>/dev/null | head -5
                echo ""
                echo "Options:"
                echo "1. Commit changes: git add . && git commit -m 'Session work'"
                echo "2. Force stop: /flowloom:session:worktree-stop force"
                echo "3. Continue working in this session"
                echo ""
                echo "❌ Session stop cancelled due to uncommitted changes"
                exit 1
            fi
        fi
        
        # Update session metadata with end time
        python3 -c "
import json
import datetime
import sys

try:
    with open('$SESSION_DIR/session.json', 'r') as f:
        session_data = json.load(f)
    
    session_data['endTime'] = datetime.datetime.utcnow().isoformat() + 'Z'
    session_data['status'] = 'completed'
    
    with open('$SESSION_DIR/session.json', 'w') as f:
        json.dump(session_data, f, indent=2)
    
    print('')
    print('📊 Final Session Details:')
    print(json.dumps(session_data, indent=2))
    
except Exception as e:
    print(f'❌ Error updating session: {e}', file=sys.stderr)
"
        
        # Change back to original directory
        if [[ -n "$ORIGINAL_DIR" ]] && [[ -d "$ORIGINAL_DIR" ]]; then
            echo ""
            echo "📁 Returning to original directory: $ORIGINAL_DIR"
            cd "$ORIGINAL_DIR"
        fi
        
        # Clean up worktree
        if [[ -n "$WORKTREE_DIR" ]] && [[ -d "$WORKTREE_DIR" ]]; then
            echo ""
            echo "🧹 Cleaning up worktree: $WORKTREE_DIR"
            
            if git worktree remove "$WORKTREE_DIR" --force 2>/dev/null; then
                echo "✅ Worktree removed successfully"
            else
                echo "⚠️  Warning: Failed to remove worktree automatically"
                echo "Manual cleanup: git worktree remove $WORKTREE_DIR --force"
            fi
        fi
        
        # Clean up session branch (optional)
        if [[ -n "$SESSION_BRANCH" ]] && [[ "$FORCE_STOP" == "true" ]]; then
            echo ""
            echo "🗑️  Removing session branch: $SESSION_BRANCH"
            if git branch -D "$SESSION_BRANCH" 2>/dev/null; then
                echo "✅ Session branch removed"
            else
                echo "⚠️  Warning: Failed to remove session branch"
                echo "Manual cleanup: git branch -D $SESSION_BRANCH"
            fi
        else
            echo ""
            echo "💡 Session branch preserved: $SESSION_BRANCH"
            echo "   Merge or delete manually if no longer needed"
        fi
        
        # Remove session pointer
        rm -f ".flowloom/current-session" 2>/dev/null
        
        # Remove from PPID registry
        CURRENT_PPID="${PPID:-$$}"
        PPID_REGISTRY=".flowloom/ppid-registry.json"
        
        python3 -c "
import json
import os

registry_file = '$PPID_REGISTRY'

try:
    if os.path.exists(registry_file):
        with open(registry_file, 'r') as f:
            registry = json.load(f)
        
        # Remove session from current PPID
        if '$CURRENT_PPID' in registry and '$CURRENT_SESSION' in registry['$CURRENT_PPID']:
            del registry['$CURRENT_PPID']['$CURRENT_SESSION']
            
            # Remove PPID entry if no sessions left
            if not registry['$CURRENT_PPID']:
                del registry['$CURRENT_PPID']
            
            with open(registry_file, 'w') as f:
                json.dump(registry, f, indent=2)
            
            print('✅ Session removed from auto-resume registry')
        
except Exception as e:
    print(f'⚠️  Warning: Could not update PPID registry: {e}')
"
        
        echo ""
        echo "✅ Worktree session stopped successfully"
        echo "📁 Current directory: $(pwd)"
        
    else
        echo "❌ Error: Session metadata not found"
        exit 1
    fi
else
    echo "ℹ️  No active session found in current directory"
    echo ""
    echo "💡 If you're in a worktree session:"
    echo "   1. cd to the worktree directory"
    echo "   2. Run this command again"
    echo ""
    echo "💡 Or specify the session ID:"
    echo "   /flowloom:session:worktree-stop <session-id>"
fi

Show the user the session stop information above including cleanup confirmation and any manual steps needed.

## Cleanup Process

**Automatic Cleanup:**
- Session metadata updated with end time
- Return to original directory
- Remove worktree from filesystem
- Remove current session pointer

**Conditional Cleanup:**
- Session branch preserved by default
- Force mode removes session branch
- Uncommitted changes prevent stop (unless forced)

**Manual Cleanup (if needed):**
- `git worktree remove <path> --force`
- `git branch -D <session-branch>`
- Remove session files manually

## Related Commands
- `/flowloom:session:worktree-start` - Start worktree session
- `/flowloom:session:status` - Check session status
- `/flowloom:session:list` - List all sessions