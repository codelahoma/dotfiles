#!/bin/bash
# Manual content synchronization

set -e

REPO_ROOT=$(git rev-parse --show-toplevel)
SYNC_SCRIPT="$REPO_ROOT/.github/scripts/sync-documentation.js"

case "${1:-all}" in
  "all")
    echo "🚀 Starting full documentation sync..."
    
    # Check if we're in the right repository
    if [[ ! -f "$SYNC_SCRIPT" ]]; then
      echo "❌ Sync script not found. Are you in the FlowLoom repository?"
      exit 1
    fi
    
    # Trigger GitHub Actions workflow
    if command -v gh &> /dev/null; then
      gh workflow run sync-documentation.yml \
        --ref main \
        --field force_sync=true \
        --field sync_reason="manual_full_sync"
      
      echo "✅ Sync workflow triggered"
      echo "📊 Monitor progress: https://github.com/codelahoma/flowloom/actions"
    else
      echo "❌ GitHub CLI not found. Install 'gh' or run sync locally."
      exit 1
    fi
    ;;
    
  "changed")
    echo "🔄 Syncing only changed documentation files..."
    
    # Get changed files in docs/ since last commit
    CHANGED_FILES=$(git diff --name-only HEAD~1 HEAD -- docs/ | tr '\n' ',' | sed 's/,$//')
    
    if [[ -z "$CHANGED_FILES" ]]; then
      echo "ℹ️  No documentation changes detected"
      exit 0
    fi
    
    echo "📝 Changed files: $CHANGED_FILES"
    
    if command -v gh &> /dev/null; then
      gh workflow run sync-documentation.yml \
        --ref main \
        --field changed_files="$CHANGED_FILES" \
        --field sync_reason="manual_changed_sync"
      
      echo "✅ Incremental sync workflow triggered"
    else
      echo "❌ GitHub CLI not found. Install 'gh' or run sync locally."
      exit 1
    fi
    ;;
    
  "local")
    echo "🖥️  Running local sync for testing..."
    
    if [[ ! -f "$SYNC_SCRIPT" ]]; then
      echo "❌ Sync script not found"
      exit 1
    fi
    
    # Check if Node.js is available
    if ! command -v node &> /dev/null; then
      echo "❌ Node.js not found. Please install Node.js to run local sync."
      exit 1
    fi
    
    # Run local sync with dry-run
    cd "$REPO_ROOT/.github/scripts"
    
    # Install dependencies if needed
    if [[ ! -d "node_modules" ]]; then
      echo "📦 Installing dependencies..."
      npm install --silent
    fi
    
    echo "🔄 Running local sync test..."
    node sync-documentation.js "$REPO_ROOT" "/tmp/flowloom-sync-test" --dry-run --verbose
    
    echo "✅ Local sync test completed"
    echo "📁 Results would be in: /tmp/flowloom-sync-test"
    echo "ℹ️  This was a dry-run. No files were actually created."
    ;;
    
  "status")
    echo "📊 Website sync status..."
    
    if command -v gh &> /dev/null; then
      echo "=== Recent Sync Workflows ==="
      gh run list --workflow=sync-documentation.yml --limit=5 \
        --json conclusion,createdAt,displayTitle,status,url \
        --template '{{range .}}{{.createdAt | timeago}} - {{.conclusion}} - {{.displayTitle}}
{{.url}}

{{end}}'
    else
      echo "❌ GitHub CLI not found. Cannot check workflow status."
    fi
    
    echo "=== Local Documentation Status ==="
    
    # Check for pending changes
    PENDING_CHANGES=$(git diff --name-only HEAD origin/main -- docs/ 2>/dev/null | wc -l || echo "0")
    if [[ $PENDING_CHANGES -gt 0 ]]; then
      echo "⚠️  $PENDING_CHANGES pending documentation changes:"
      git diff --name-only HEAD origin/main -- docs/ 2>/dev/null || echo "Unable to compare with origin/main"
    else
      echo "✅ Documentation appears up to date"
    fi
    
    # Check last local commit affecting docs
    LAST_DOC_COMMIT=$(git log -1 --format="%h %s (%cr)" -- docs/ 2>/dev/null || echo "No documentation commits found")
    echo "📝 Last documentation change: $LAST_DOC_COMMIT"
    ;;
    
  "validate")
    echo "🔍 Validating local documentation..."
    
    if [[ ! -f "$REPO_ROOT/.github/scripts/validate-content.js" ]]; then
      echo "❌ Validation script not found"
      exit 1
    fi
    
    cd "$REPO_ROOT/.github/scripts"
    
    # Install dependencies if needed
    if [[ ! -d "node_modules" ]]; then
      echo "📦 Installing dependencies..."
      npm install --silent
    fi
    
    echo "🔍 Running comprehensive validation..."
    node validate-content.js "$REPO_ROOT/docs" --verbose
    
    VALIDATION_EXIT_CODE=$?
    
    if [[ $VALIDATION_EXIT_CODE -eq 0 ]]; then
      echo "✅ All validation checks passed!"
    else
      echo "❌ Validation found issues. Please review and fix before syncing."
      exit $VALIDATION_EXIT_CODE
    fi
    ;;
    
  *)
    echo "Usage: /flowloom:website:sync [all|changed|local|status|validate]"
    echo ""
    echo "Commands:"
    echo "  all      - Full documentation sync (triggers GitHub Actions)"
    echo "  changed  - Sync only changed files since last commit"
    echo "  local    - Run local test sync with dry-run"
    echo "  status   - Show sync status and pending changes"
    echo "  validate - Validate local documentation before sync"
    echo ""
    echo "Examples:"
    echo "  /flowloom:website:sync all       # Full sync"
    echo "  /flowloom:website:sync changed   # Incremental sync"
    echo "  /flowloom:website:sync local     # Test locally"
    echo "  /flowloom:website:sync validate  # Check for issues"
    ;;
esac