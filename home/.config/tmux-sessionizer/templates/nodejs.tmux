#!/usr/bin/env bash
# Node.js project tmux template
# Split: Editor (70%) | Terminal for npm commands (30%)

# Get session name from first argument
SESSION="$1"

# Split window horizontally: left (70%) for editing, right (30%) for terminal
tmux split-window -h -t "$SESSION:1" -p 30

# Show npm scripts hint in right pane
tmux send-keys -t "$SESSION:1.2" 'echo "💡 Tip: npm run <tab> to see available scripts"' C-m

# Focus left pane (editor)
tmux select-pane -t "$SESSION:1.1"
