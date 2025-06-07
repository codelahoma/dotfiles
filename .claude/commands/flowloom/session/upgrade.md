Let input_args = "$ARGUMENTS"

# FlowLoom Session Upgrade Command

You will upgrade all FlowLoom sessions by merging changes from the main branch into each session directory. This ensures all sessions have the latest FlowLoom improvements and configurations.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Upgrade all sessions in sessions/ directory
- If input_args contains a session ID: Upgrade only that specific session
- If input_args is "list": Show available sessions without upgrading
- If input_args is "dry-run": Show what would be upgraded without making changes

## Argument Patterns
- (no arguments) - Upgrade all sessions
- `<session-id>` - Upgrade specific session (e.g., "19778-development")
- `list` - List available sessions
- `dry-run` - Preview upgrade actions without executing

## Session Upgrade Process

### Step 1: List Available Sessions
@bash
echo "=== Available FlowLoom Sessions ==="
if [ -d "sessions" ]; then
    cd sessions
    for session_dir in */; do
        if [ -d "$session_dir" ]; then
            session_name=$(basename "$session_dir")
            echo "📁 $session_name"
            cd "$session_dir"
            if git rev-parse --git-dir >/dev/null 2>&1; then
                current_branch=$(git branch --show-current 2>/dev/null || echo "unknown")
                git_type=$(if [ -d ".git" ]; then echo "repository"; else echo "worktree"; fi)
                echo "   └── Branch: $current_branch ($git_type)"
            else
                echo "   └── No git repository found"
            fi
            cd ..
        fi
    done
    cd ..
else
    echo "❌ No sessions directory found"
    exit 1
fi

Display the session listing above to the user, including branch information for each session.

### Step 2: Determine Sessions to Upgrade
Based on the input arguments, determine which sessions to process:
- If "list" was specified, stop here and show only the listing
- If "dry-run" was specified, prepare to show upgrade actions without executing
- If a specific session ID was provided, validate it exists
- If no arguments, prepare to upgrade all sessions

### Step 3: Execute Session Upgrades
For each session to be upgraded:

@bash
echo "=== Starting Session Upgrade Process ==="

# Store current directory
ORIGINAL_DIR=$(pwd)

# Function to upgrade a single session
upgrade_session() {
    local session_name="$1"
    local is_dry_run="$2"
    
    echo "🔄 Processing session: $session_name"
    
    if [ ! -d "sessions/$session_name" ]; then
        echo "❌ Session directory not found: sessions/$session_name"
        return 1
    fi
    
    cd "sessions/$session_name"
    
    # Check if it's a git repository or worktree
    if ! git rev-parse --git-dir >/dev/null 2>&1; then
        echo "⚠️  No git repository in sessions/$session_name - skipping"
        cd "$ORIGINAL_DIR"
        return 1
    fi
    
    # Get current branch
    current_branch=$(git branch --show-current 2>/dev/null)
    if [ -z "$current_branch" ]; then
        echo "❌ Could not determine current branch in sessions/$session_name"
        cd "$ORIGINAL_DIR"
        return 1
    fi
    
    echo "   Current branch: $current_branch"
    
    # Check for uncommitted changes
    if ! git diff --quiet HEAD 2>/dev/null; then
        echo "⚠️  Uncommitted changes detected in sessions/$session_name"
        if [ "$is_dry_run" = "true" ]; then
            echo "   [DRY RUN] Would stash changes before merge"
        else
            echo "   Stashing changes before merge..."
            git stash push -m "Auto-stash before FlowLoom upgrade $(date)"
        fi
    fi
    
    # Fetch latest changes from main
    if [ "$is_dry_run" = "true" ]; then
        echo "   [DRY RUN] Would fetch origin main"
        echo "   [DRY RUN] Would merge origin/main into $current_branch"
    else
        echo "   Fetching latest changes from main..."
        if git fetch origin main 2>/dev/null; then
            echo "   Merging main into $current_branch..."
            if git merge origin/main --no-edit; then
                echo "✅ Successfully upgraded sessions/$session_name"
            else
                echo "❌ Merge conflict in sessions/$session_name - manual resolution required"
                echo "   You can resolve conflicts and complete the merge manually"
            fi
        else
            echo "❌ Failed to fetch from origin for sessions/$session_name"
        fi
    fi
    
    cd "$ORIGINAL_DIR"
    echo ""
}

# Process sessions based on arguments
if echo "$input_args" | grep -q "dry-run"; then
    echo "🔍 DRY RUN MODE - No changes will be made"
    IS_DRY_RUN="true"
else
    IS_DRY_RUN="false"
fi

if [ -z "$input_args" ] || echo "$input_args" | grep -q "dry-run"; then
    # Upgrade all sessions
    echo "Upgrading all sessions..."
    if [ -d "sessions" ]; then
        cd sessions
        for session_dir in */; do
            if [ -d "$session_dir" ]; then
                session_name=$(basename "$session_dir")
                cd "$ORIGINAL_DIR"
                upgrade_session "$session_name" "$IS_DRY_RUN"
            fi
        done
        cd "$ORIGINAL_DIR"
    fi
elif echo "$input_args" | grep -v -q "list"; then
    # Upgrade specific session
    session_id="$input_args"
    upgrade_session "$session_id" "$IS_DRY_RUN"
fi

echo "=== Session Upgrade Complete ==="

Display the upgrade results above to the user, including any warnings about merge conflicts or failed operations.

### Step 4: Post-Upgrade Summary
After completing the upgrades, provide a summary of:
- Number of sessions processed
- Number of successful upgrades
- Any sessions that require manual attention
- Next steps for resolving any issues

## Usage Examples
- `/flowloom:session:upgrade` - Upgrade all sessions
- `/flowloom:session:upgrade 19778-development` - Upgrade specific session
- `/flowloom:session:upgrade list` - List available sessions
- `/flowloom:session:upgrade dry-run` - Preview upgrade actions

## Important Notes
- Sessions with uncommitted changes will have them stashed automatically
- Merge conflicts require manual resolution
- Each session maintains its own branch and git history
- The upgrade preserves session-specific configurations and changes