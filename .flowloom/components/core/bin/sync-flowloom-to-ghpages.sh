#!/bin/bash
# FlowLoom System Sync Script
# Syncs FlowLoom development system from main to gh-pages branch

set -e

echo "🔄 Syncing FlowLoom system from main to gh-pages..."

# Store current branch
CURRENT_BRANCH=$(git branch --show-current)

# Switch to gh-pages
echo "📝 Switching to gh-pages branch..."
git checkout gh-pages

# Sync core FlowLoom files from main branch
echo "📦 Syncing FlowLoom configuration..."
git checkout main -- \
  CLAUDE.md \
  CLAUDE.local.md \
  base_project.md \
  .claude/ \
  .mcp.json \
  memory.json \
  flowloom.json \
  MCP_SETUP.md \
  README.md \
  "The Story of FlowLoom.md" \
  plans/ \
  prompts/ \
  flowloom-docker/ \
  walkthroughs/

echo "✅ FlowLoom system synced to gh-pages"
echo "📋 Files synced:"
echo "   - Configuration: CLAUDE.md, .claude/, .mcp.json"
echo "   - Project data: plans/, memory.json, flowloom.json"
echo "   - Development: prompts/, flowloom-docker/, walkthroughs/"
echo ""
echo "Next steps:"
echo "   1. Review changes: git status"
echo "   2. Commit if needed: git add . && git commit -m 'sync: Update FlowLoom system'"
echo "   3. Push to deploy: git push origin gh-pages"
echo "   4. Return to main: git checkout main"

# Note: Don't auto-commit to allow user review
echo ""
echo "🔍 Current git status:"
git status --short