#!/usr/bin/env bash
# Rust project tmux template
# Split: Editor (70%) | Terminal for cargo commands (30%)

# Get session name from first argument
SESSION="$1"

# Split window horizontally: left (70%) for editing, right (30%) for terminal
tmux split-window -h -t "$SESSION:1" -p 30

# Show cargo hint in right pane
tmux send-keys -t "$SESSION:1.2" 'echo "🦀 Rust project"' C-m
tmux send-keys -t "$SESSION:1.2" 'echo "  cargo build   - Build project"' C-m
tmux send-keys -t "$SESSION:1.2" 'echo "  cargo run     - Run project"' C-m
tmux send-keys -t "$SESSION:1.2" 'echo "  cargo test    - Run tests"' C-m
tmux send-keys -t "$SESSION:1.2" 'echo "  cargo check   - Quick check"' C-m

# Focus left pane (editor)
tmux select-pane -t "$SESSION:1.1"
