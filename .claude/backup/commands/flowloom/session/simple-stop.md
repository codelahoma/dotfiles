Let input_args = "$ARGUMENTS"

# Simple Session Stop Command

End the current simple session and clean up tracking.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Stop current session
- If input_args contains session ID: Stop specific session
- If input_args contains "force": Force stop even with errors

## Implementation

@bash
if [[ -f ".flowloom/current-session" ]]; then
    CURRENT_SESSION=$(cat .flowloom/current-session)
    echo "🛑 Stopping Session: $CURRENT_SESSION"
    echo "=================================="
    
    if [[ -f ".flowloom/sessions/$CURRENT_SESSION/session.json" ]]; then
        # Add end time to session using Python
        python3 -c "
import json
import datetime
import sys

try:
    with open('.flowloom/sessions/$CURRENT_SESSION/session.json', 'r') as f:
        session_data = json.load(f)
    
    session_data['endTime'] = datetime.datetime.utcnow().isoformat() + 'Z'
    session_data['status'] = 'completed'
    
    with open('.flowloom/sessions/$CURRENT_SESSION/session.json', 'w') as f:
        json.dump(session_data, f, indent=2)
    
    print('✅ Session ended successfully')
    print('')
    print('Final Session Details:')
    print(json.dumps(session_data, indent=2))
    
except Exception as e:
    print(f'❌ Error updating session: {e}', file=sys.stderr)
    sys.exit(1)
"
        
        # Remove current session pointer
        rm -f ".flowloom/current-session"
        
        echo ""
        echo "📝 Session data preserved in: .flowloom/sessions/$CURRENT_SESSION/"
    else
        echo "⚠️  Session data not found, cleaning up pointer"
        rm -f ".flowloom/current-session"
    fi
else
    echo "ℹ️  No active session to stop"
fi

Show the user the session stop information above including final session details and cleanup confirmation.

## Session Cleanup

**What gets cleaned up:**
- Current session pointer (`.flowloom/current-session`)
- Session marked as completed with end time

**What gets preserved:**
- Session metadata and history
- All session files and artifacts
- Session directory for future reference

## Related Commands
- `/flowloom:session:simple-start` - Start a new simple session
- `/flowloom:session:status` - Check session status
- `/flowloom:session:list` - List all sessions