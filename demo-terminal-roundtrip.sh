#!/bin/bash

echo "🎯 FlowLoom UI Round-Trip Demo"
echo "=============================="
echo
echo "This demo shows full round-trip communication:"
echo "UI → File → Terminal Bridge → Claude Code → Response"
echo
echo "Steps:"
echo "1. Open a new Terminal window"
echo "2. Run 'claude' in that Terminal"
echo "3. Run this script"
echo "4. Use the FlowLoom UI"
echo
read -p "Press Enter when you have Claude running in Terminal..."

echo
echo "Starting Terminal Bridge..."
python3 bin/flowloom-terminal-bridge.py &
BRIDGE_PID=$!

echo "Bridge PID: $BRIDGE_PID"
echo

sleep 2

echo "Launching FlowLoom UI..."
./FlowLoomUI-Redesigned.swift &
UI_PID=$!

echo "UI PID: $UI_PID"
echo
echo "✅ Demo is running!"
echo
echo "Try these in the UI:"
echo "  - Type: plan:create 'Todo App'"
echo "  - Say: 'Hey Flo, create a project plan'"
echo "  - Type: 'Show me Swift code for a REST client'"
echo
echo "The commands will appear in your Terminal with Claude!"
echo
echo "Press Ctrl+C to stop the demo"

# Wait for Ctrl+C
trap "echo 'Stopping...'; kill $BRIDGE_PID $UI_PID 2>/dev/null; exit" INT
wait