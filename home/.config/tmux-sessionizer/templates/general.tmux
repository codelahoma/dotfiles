#!/usr/bin/env bash
# General project tmux template
# Simple split: Editor (70%) | Terminal (30%)

# Get session name from first argument
SESSION="$1"

# Split window horizontally: left (70%) for editing, right (30%) for terminal
tmux split-window -h -t "$SESSION:1" -p 30

# Focus left pane (editor)
tmux select-pane -t "$SESSION:1.1"
