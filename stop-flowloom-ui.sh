#!/bin/bash

# FlowLoom UI Stopper
# Cleanly stops the UI and Terminal Bridge

echo "🛑 Stopping FlowLoom UI..."

LOG_DIR="./logs"

# Read PIDs
if [ -f "$LOG_DIR/bridge.pid" ]; then
    BRIDGE_PID=$(cat "$LOG_DIR/bridge.pid")
    if kill -0 "$BRIDGE_PID" 2>/dev/null; then
        kill "$BRIDGE_PID"
        echo "   ✓ Stopped Terminal Bridge (PID: $BRIDGE_PID)"
    fi
    rm "$LOG_DIR/bridge.pid"
fi

if [ -f "$LOG_DIR/ui.pid" ]; then
    UI_PID=$(cat "$LOG_DIR/ui.pid")
    if kill -0 "$UI_PID" 2>/dev/null; then
        kill "$UI_PID"
        echo "   ✓ Stopped FlowLoom UI (PID: $UI_PID)"
    fi
    rm "$LOG_DIR/ui.pid"
fi

# Also kill any Swift processes running FlowLoom UI
SWIFT_PIDS=$(pgrep -f "FlowLoomUI-Enhanced.swift" || true)
if [ -n "$SWIFT_PIDS" ]; then
    for pid in $SWIFT_PIDS; do
        kill "$pid" 2>/dev/null || true
        echo "   ✓ Stopped Swift FlowLoom UI process (PID: $pid)"
    done
fi

# Also kill any swift-frontend processes running FlowLoom
FRONTEND_PIDS=$(pgrep -f "swift-frontend.*FlowLoomUI" || true)
if [ -n "$FRONTEND_PIDS" ]; then
    for pid in $FRONTEND_PIDS; do
        kill "$pid" 2>/dev/null || true
        echo "   ✓ Stopped swift-frontend FlowLoom process (PID: $pid)"
    done
fi

echo "✅ FlowLoom UI stopped"