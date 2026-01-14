#!/usr/bin/env bash
# Go project tmux template
# Split: Editor (70%) | Terminal for go commands (30%)

# Get session name from first argument
SESSION="$1"

# Split window horizontally: left (70%) for editing, right (30%) for terminal
tmux split-window -h -t "$SESSION:1" -p 30

# Show go hints in right pane
tmux send-keys -t "$SESSION:1.2" 'echo "🐹 Go project"' C-m
tmux send-keys -t "$SESSION:1.2" 'echo "  go build      - Build project"' C-m
tmux send-keys -t "$SESSION:1.2" 'echo "  go run .      - Run project"' C-m
tmux send-keys -t "$SESSION:1.2" 'echo "  go test ./... - Run tests"' C-m

# Focus left pane (editor)
tmux select-pane -t "$SESSION:1.1"
