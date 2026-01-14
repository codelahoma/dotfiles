#!/usr/bin/env bash
# Dotfiles project tmux template
# Layout: Editor (top 60%) | Shell (bottom-left 20%) | Git status (bottom-right 20%)

# Get session name from first argument
SESSION="$1"

# Split horizontally: top (60%) for editor, bottom (40%) for commands
tmux split-window -v -t "$SESSION:1" -p 40

# Split bottom pane vertically: left for shell, right for git status
tmux split-window -h -t "$SESSION:1.2"

# Setup git status pane (right bottom) with watch
tmux send-keys -t "$SESSION:1.3" 'watch -n 2 -c "git status --short --branch"' C-m

# Focus top pane (editor)
tmux select-pane -t "$SESSION:1.1"
