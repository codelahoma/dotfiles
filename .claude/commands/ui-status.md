# FlowLoom UI Status

Check the status of FlowLoom UI processes.

```bash
echo "🔍 FlowLoom UI Process Status"
echo "============================"
echo

# Check UI process
if pgrep -f "FlowLoomUI" > /dev/null; then
    echo "✅ FlowLoom UI: Running"
    ps aux | grep -E "FlowLoomUI" | grep -v grep | awk '{print "   PID: " $2 ", CPU: " $3 "%, Memory: " $4 "%"}'
else
    echo "❌ FlowLoom UI: Not running"
fi

# Check Terminal Bridge
if pgrep -f "terminal-bridge" > /dev/null; then
    echo "✅ Terminal Bridge: Running"
    ps aux | grep -E "terminal-bridge" | grep -v grep | awk '{print "   PID: " $2 ", CPU: " $3 "%, Memory: " $4 "%"}'
else
    echo "❌ Terminal Bridge: Not running"
fi

echo
echo "📁 Log files:"
echo "   UI: ~/.flowloom/logs/flowloom-ui.log"
echo "   Bridge: ~/.flowloom/logs/terminal-bridge.log"
```

📊 FlowLoom UI status checked.