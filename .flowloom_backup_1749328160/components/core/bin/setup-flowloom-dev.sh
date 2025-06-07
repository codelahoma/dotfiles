#!/bin/bash

# FlowLoom Development Environment Setup
# Sets up environment for FlowLoom development by configuring FLOWLOOM_WORK_DIR

set -e

echo "🌊 FlowLoom Development Environment Setup"
echo "========================================"
echo

# Check if we're in a FlowLoom repository
if [ ! -f "CLAUDE.md" ] || [ ! -d "plans" ]; then
    echo "❌ Error: This doesn't appear to be a FlowLoom repository"
    echo "   Expected to find CLAUDE.md and plans/ directory"
    exit 1
fi

echo "✅ FlowLoom repository detected"
echo

# Set up development environment
export FLOWLOOM_WORK_DIR="."
echo "🔧 Setting FLOWLOOM_WORK_DIR to current directory (.)"
echo "   This enables FlowLoom development mode"
echo

# Verify setup
WORK_DIR="${FLOWLOOM_WORK_DIR:-.meta-claude}"
echo "📂 Current work directory: $WORK_DIR"

# Check for required directories
required_dirs=("plans" "docs" "bin")
all_exist=true

for dir in "${required_dirs[@]}"; do
    if [ -d "$WORK_DIR/$dir" ]; then
        echo "   ✅ $WORK_DIR/$dir exists"
    else
        echo "   ⚠️  $WORK_DIR/$dir does not exist"
        all_exist=false
    fi
done

echo

if [ "$all_exist" = true ]; then
    echo "🎉 FlowLoom development environment ready!"
else
    echo "⚠️  Some directories are missing but this is normal for initial setup"
fi

echo
echo "📝 To make this permanent, add this to your shell profile:"
echo "   export FLOWLOOM_WORK_DIR=\".\""
echo
echo "   For bash: echo 'export FLOWLOOM_WORK_DIR=\".\"' >> ~/.bashrc"
echo "   For zsh:  echo 'export FLOWLOOM_WORK_DIR=\".\"' >> ~/.zshrc"
echo
echo "🔄 To test the setup, run: ./bin/validate-flowloom-env.sh"