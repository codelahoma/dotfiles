#!/usr/bin/env bash
# Python project tmux template
# Split: Editor (70%) | Terminal with virtualenv (30%)

# Get session name from first argument
SESSION="$1"

# Split window horizontally: left (70%) for editing, right (30%) for terminal
tmux split-window -h -t "$SESSION:1" -p 30

# Activate virtualenv in right pane if it exists
tmux send-keys -t "$SESSION:1.2" 'if [ -d .venv ]; then source .venv/bin/activate; elif [ -d venv ]; then source venv/bin/activate; fi' C-m
tmux send-keys -t "$SESSION:1.2" 'clear' C-m

# Focus left pane (editor)
tmux select-pane -t "$SESSION:1.1"
