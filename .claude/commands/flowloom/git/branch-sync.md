Let input_args = "$ARGUMENTS"

# FlowLoom Branch Synchronization

Perform bidirectional sync operations between main and gh-pages branches using the enhanced sync script.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Run interactive mode for branch sync
- If input_args contains "all": Perform complete sync (website + commands + config backup)
- If input_args contains "web": Auto-detect branch and sync to website (main→gh-pages only)
- If input_args contains "main-to-gh" or "main": Sync main branch to gh-pages
- If input_args contains "gh-to-main" or "gh": Sync gh-pages to main branch
- If input_args contains "analyze" or "diff": Analyze differences without syncing
- If input_args contains "force": Force sync without conflict resolution
- If input_args contains "dry-run": Preview changes without applying them
- If input_args contains "backup": Include backup location

## Argument Patterns
- (no arguments) - Interactive mode with conflict resolution
- "all" - Complete sync: website + commands + config backup (main→gh-pages only)
- "web" - Auto-detect and sync to website (main→gh-pages only)
- "main-to-gh" or "main" - Sync main → gh-pages
- "gh-to-main" or "gh" - Sync gh-pages → main
- "analyze" or "diff" - Show branch differences only
- "force" - Force sync overriding conflicts
- "dry-run" - Preview mode (show what would change)
- "backup" - Create backup before sync

## Enhanced Branch Sync Process

### Step 1: Parse Arguments and Set Options
Determine sync direction and options based on input arguments:

@bash
SYNC_ARGS=""
DIRECTION=""

# Parse input arguments
if echo "$input_args" | grep -q "all"; then
  DIRECTION="all"
elif echo "$input_args" | grep -q "web"; then
  DIRECTION="web"
elif echo "$input_args" | grep -q "main-to-gh\|main"; then
  DIRECTION="main-to-gh"
elif echo "$input_args" | grep -q "gh-to-main\|gh"; then
  DIRECTION="gh-to-main"
elif echo "$input_args" | grep -q "analyze\|diff"; then
  DIRECTION="analyze"
else
  DIRECTION="interactive"
fi

# Add options
if echo "$input_args" | grep -q "force"; then
  SYNC_ARGS="$SYNC_ARGS --force"
fi

if echo "$input_args" | grep -q "dry-run"; then
  SYNC_ARGS="$SYNC_ARGS --dry-run"
fi

if echo "$input_args" | grep -q "backup"; then
  SYNC_ARGS="$SYNC_ARGS --backup /tmp"
fi

echo "🔄 FlowLoom Branch Sync"
echo "======================"
echo "Direction: $DIRECTION"
echo "Options: $SYNC_ARGS"
echo ""

Show the user the sync configuration above.

### Step 2: Execute Sync Operations
Run the appropriate sync operations based on the direction:

@bash
if [ "$DIRECTION" = "all" ]; then
  echo "🚀 COMPLETE SYNC: Running all sync operations..."
  echo "="*50
  echo "Phase 1: Website Sync (main→gh-pages)"
  ./bin/sync-flowloom-bidirectional.sh web $SYNC_ARGS
  echo ""
  echo "Phase 2: Commands and Configuration Sync"
  ./bin/sync_claude_commands.sh --verbose
  echo ""
  echo "✅ Complete sync finished!"
else
  ./bin/sync-flowloom-bidirectional.sh $DIRECTION $SYNC_ARGS
fi

Show the user the complete output from the sync operation above, including:
- Branch analysis results (if applicable)
- Website sync results (if applicable)
- Commands sync results (if applicable)
- Conflict detection (if any)
- Files that would be synced or were synced
- Any warnings or errors
- Next steps recommendations

### Step 3: Provide Guidance
Based on the sync results, provide appropriate next steps:

If conflicts were detected:
- Suggest using `force` option to override
- Recommend manual conflict resolution
- Explain the interactive mode option

If sync was successful:
- Suggest reviewing changes with `git status`
- Recommend committing and pushing changes
- Mention backup location if created

If this was analysis only:
- Summarize the differences found
- Suggest specific sync direction based on findings
- Offer to perform the actual sync

## Safety Features

The enhanced sync script includes:
- ✅ **Conflict Detection** - Identifies files modified in both branches
- ✅ **Backup Creation** - Automatic backups before destructive operations
- ✅ **Dry Run Mode** - Preview changes without applying them
- ✅ **Interactive Resolution** - Step-by-step conflict handling
- ✅ **Branch Restoration** - Returns to original branch on completion
- ✅ **Force Override** - Option to resolve conflicts by overwriting

## Usage Examples

```bash
# Complete sync: website + commands + config (recommended for deployment)
/flowloom:git:branch-sync all

# Auto-detect and sync to website only
/flowloom:git:branch-sync web

# Interactive mode (for complex scenarios)
/flowloom:git:branch-sync

# Analyze differences between branches
/flowloom:git:branch-sync analyze

# Preview complete sync without changes
/flowloom:git:branch-sync all dry-run

# Force complete sync overriding conflicts
/flowloom:git:branch-sync all force backup

# Sync main to gh-pages with backup
/flowloom:git:branch-sync main-to-gh backup
```

This command provides safe, comprehensive synchronization with automatic branch detection for website deployment, conflict detection, backup creation, and multiple operation modes for different use cases.

**Sync Modes:**
- **all**: Complete deployment (website + commands + config) - recommended for full deployment
- **web**: Website deployment only (main→gh-pages)
- **Interactive**: Step-by-step conflict resolution for complex scenarios

The all sync option is the recommended approach for complete FlowLoom system deployment to the website.