#!/bin/bash

# Claude with FlowLoom UI
# Starts Claude and automatically launches the UI in background

echo "🤖 Claude with FlowLoom UI"
echo "========================="
echo

# Function to cleanup on exit
cleanup() {
    echo
    echo "Shutting down FlowLoom UI..."
    ./stop-flowloom-ui.sh 2>/dev/null
    exit
}

# Set up trap for clean exit
trap cleanup EXIT INT TERM

# Launch FlowLoom UI in background
echo "Launching FlowLoom UI in background..."
./launch-flowloom-ui.sh

# Small delay to let UI start
sleep 2

echo
echo "Starting Claude..."
echo "The FlowLoom UI is running - try voice commands with 'Hey Flo'!"
echo

# Run Claude in foreground
claude "$@"