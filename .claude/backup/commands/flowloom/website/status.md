#!/bin/bash
# Website health and sync status

set -e

REPO_ROOT=$(git rev-parse --show-toplevel)

echo "=== FlowLoom Website Status ==="
echo ""

# Website health check
echo "🌐 Website Health Check:"
if command -v curl &> /dev/null; then
  # Check main pages
  curl -s -o /dev/null -w "Homepage: %{http_code} (%{time_total}s)\n" https://flowloom.dev/ || echo "Homepage: Connection failed"
  curl -s -o /dev/null -w "Docs: %{http_code} (%{time_total}s)\n" https://flowloom.dev/docs/ || echo "Docs: Connection failed"
  curl -s -o /dev/null -w "Sitemap: %{http_code} (%{time_total}s)\n" https://flowloom.dev/sitemap.xml || echo "Sitemap: Connection failed"
else
  echo "❌ curl not available - cannot check website health"
fi

echo ""

# Sync workflow status
echo "📊 Sync Workflow Status:"
if command -v gh &> /dev/null; then
  # Get last sync workflow run
  LAST_SYNC=$(gh api repos/codelahoma/flowloom/actions/workflows/sync-documentation.yml/runs --jq '.workflow_runs[0]' 2>/dev/null || echo "{}")
  
  if [[ "$LAST_SYNC" != "{}" ]]; then
    SYNC_STATUS=$(echo "$LAST_SYNC" | jq -r '.conclusion // "in_progress"')
    SYNC_TIME=$(echo "$LAST_SYNC" | jq -r '.created_at')
    SYNC_URL=$(echo "$LAST_SYNC" | jq -r '.html_url')
    
    echo "Last sync: $SYNC_TIME"
    echo "Status: $SYNC_STATUS"
    echo "URL: $SYNC_URL"
  else
    echo "No sync workflows found"
  fi
  
  echo ""
  echo "Recent sync runs:"
  gh run list --workflow=sync-documentation.yml --limit=3 \
    --json conclusion,createdAt,displayTitle \
    --template '{{range .}}{{.createdAt | timeago}} - {{.conclusion}} - {{.displayTitle}}{{"\n"}}{{end}}' 2>/dev/null || echo "Unable to fetch recent runs"
else
  echo "❌ GitHub CLI not available - install 'gh' for workflow status"
fi

echo ""

# Documentation freshness
echo "📚 Documentation Status:"
if [[ -d "$REPO_ROOT/docs" ]]; then
  # Count documentation files
  DOC_COUNT=$(find "$REPO_ROOT/docs" -name "*.md" | wc -l)
  echo "Documentation files: $DOC_COUNT"
  
  # Last documentation change
  LAST_DOC_COMMIT=$(git log -1 --format="%h %s (%cr)" -- docs/ 2>/dev/null || echo "No documentation commits found")
  echo "Last change: $LAST_DOC_COMMIT"
  
  # Check for uncommitted changes
  if git diff --quiet docs/ 2>/dev/null; then
    echo "Uncommitted changes: None"
  else
    CHANGED_FILES=$(git diff --name-only docs/ | wc -l)
    echo "Uncommitted changes: $CHANGED_FILES files"
    git diff --name-only docs/ | head -5 | sed 's/^/  - /'
    if [[ $CHANGED_FILES -gt 5 ]]; then
      echo "  ... and $((CHANGED_FILES - 5)) more files"
    fi
  fi
  
  # Check for staged changes
  if git diff --cached --quiet docs/ 2>/dev/null; then
    echo "Staged changes: None"
  else
    STAGED_FILES=$(git diff --cached --name-only docs/ | wc -l)
    echo "Staged changes: $STAGED_FILES files"
  fi
else
  echo "❌ No docs directory found"
fi

echo ""

# Sync configuration
echo "⚙️  Sync Configuration:"
if [[ -f "$REPO_ROOT/.github/workflows/sync-documentation.yml" ]]; then
  echo "✅ Sync workflow configured"
  
  # Check for required scripts
  if [[ -f "$REPO_ROOT/.github/scripts/sync-documentation.js" ]]; then
    echo "✅ Sync script available"
  else
    echo "❌ Sync script missing"
  fi
  
  if [[ -f "$REPO_ROOT/.github/scripts/validate-content.js" ]]; then
    echo "✅ Validation script available"
  else
    echo "❌ Validation script missing"
  fi
  
  # Check Node.js dependencies
  if [[ -f "$REPO_ROOT/.github/scripts/package.json" ]]; then
    echo "✅ Dependencies configured"
    
    if [[ -d "$REPO_ROOT/.github/scripts/node_modules" ]]; then
      echo "✅ Dependencies installed"
    else
      echo "⚠️  Dependencies not installed (run npm install in .github/scripts/)"
    fi
  else
    echo "❌ Dependencies configuration missing"
  fi
else
  echo "❌ Sync workflow not configured"
fi

echo ""

# GitHub Pages status
echo "🚀 GitHub Pages Status:"
if command -v gh &> /dev/null; then
  # Get Pages deployment status
  PAGES_STATUS=$(gh api repos/codelahoma/flowloom/pages 2>/dev/null | jq -r '.status // "unknown"' || echo "unknown")
  echo "Pages status: $PAGES_STATUS"
  
  # Get recent deployments
  echo "Recent deployments:"
  gh api repos/codelahoma/flowloom/deployments \
    --jq '.[] | select(.environment == "github-pages") | "\(.created_at | fromdate | strftime("%Y-%m-%d %H:%M")) - \(.deployment_status.state // "pending")"' \
    2>/dev/null | head -3 || echo "Unable to fetch deployment status"
else
  echo "❌ GitHub CLI not available for Pages status"
fi

echo ""

# Quick actions
echo "🛠️  Quick Actions:"
echo "Run full sync:       /flowloom:website:sync all"
echo "Sync changes only:   /flowloom:website:sync changed"
echo "Test locally:        /flowloom:website:sync local"
echo "Validate docs:       /flowloom:website:sync validate"
echo "Website preview:     /flowloom:website:preview"