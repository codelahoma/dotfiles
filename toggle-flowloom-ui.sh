#!/bin/bash

# FlowLoom UI Toggle
# Shows/hides the FlowLoom UI window without stopping the process

UI_PID_FILE="$HOME/.flowloom/logs/ui.pid"

echo "🔄 FlowLoom UI Toggle"
echo "===================="

# Check if UI is running
if [ -f "$UI_PID_FILE" ]; then
    UI_PID=$(cat "$UI_PID_FILE")
    
    if ps -p "$UI_PID" > /dev/null 2>&1; then
        echo "📱 FlowLoom UI is running (PID: $UI_PID)"
        
        # Use AppleScript to toggle window visibility
        osascript << 'EOF'
tell application "System Events"
    set appName to "FlowLoomUI-Redesigned"
    if (count of (every process whose name is appName)) > 0 then
        tell application appName
            if visible then
                set visible to false
                return "🙈 FlowLoom UI hidden"
            else
                set visible to true
                activate
                return "👁️ FlowLoom UI shown"
            end if
        end tell
    else
        return "❌ FlowLoom UI process not found"
    end if
end tell
EOF
        
    else
        echo "❌ FlowLoom UI not running (stale PID file)"
        rm -f "$UI_PID_FILE"
    fi
else
    echo "❌ FlowLoom UI not running"
    echo "   Start with: ./launch-flowloom-ui.sh"
fi

echo