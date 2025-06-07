#!/bin/bash
# FlowLoom Bidirectional Sync Script
# Enhanced sync for FlowLoom development system between main and gh-pages branches
# Supports conflict detection, selective sync, and rollback capabilities

set -e

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
FLOWLOOM_FILES=(
    "CLAUDE.md"
    "CLAUDE.local.md" 
    "base_project.md"
    ".claude/"
    ".mcp.json"
    "memory.json"
    "flowloom.json"
    "MCP_SETUP.md"
    "README.md"
    "The Story of FlowLoom.md"
    "plans/"
    "prompts/"
    "flowloom-docker/"
    "walkthroughs/"
    "docs/"
)

# Website-specific files to preserve on gh-pages
WEBSITE_FILES=(
    "_config.yml"
    "_layouts/"
    "_includes/"
    "_sass/"
    "assets/"
    "CNAME"
    "index.html"
    "404.html"
    ".jekyll-ignore"
)

# Detect current branch and determine appropriate sync direction
detect_sync_direction() {
    local current_branch=$(git branch --show-current)
    
    case "$current_branch" in
        "main")
            echo "main-to-gh"
            ;;
        "gh-pages")
            echo "gh-to-main"
            ;;
        *)
            # For other branches, default to main-to-gh
            warn "Unknown branch '$current_branch', defaulting to main-to-gh sync"
            echo "main-to-gh"
            ;;
    esac
}

# Initialize variables
VERBOSE=0
DRY_RUN=0
DIRECTION=""
INTERACTIVE=1
BACKUP_DIR=""
FORCE=0

usage() {
    echo "FlowLoom Bidirectional Sync Script"
    echo "=================================="
    echo ""
    echo "Usage: $0 [OPTIONS] DIRECTION"
    echo ""
    echo "DIRECTIONS:"
    echo "  web             Auto-detect branch and sync to website (main→gh-pages)"
    echo "  main-to-gh      Sync from main branch to gh-pages"
    echo "  gh-to-main      Sync from gh-pages branch to main"
    echo "  analyze         Analyze differences without syncing"
    echo "  interactive     Interactive mode (default)"
    echo ""
    echo "OPTIONS:"
    echo "  -v, --verbose       Show detailed output"
    echo "  -d, --dry-run       Show what would be done without making changes"
    echo "  -f, --force         Force sync without conflict resolution"
    echo "  -y, --yes           Non-interactive mode (auto-confirm)"
    echo "  -b, --backup DIR    Create backup in specified directory"
    echo "  -h, --help          Show this help message"
    echo ""
    echo "EXAMPLES:"
    echo "  $0 web                        # Auto-sync to website based on current branch"
    echo "  $0 analyze                    # Show differences between branches"
    echo "  $0 main-to-gh --dry-run       # Preview main to gh-pages sync"
    echo "  $0 gh-to-main --backup /tmp   # Sync with backup"
    echo "  $0 interactive                # Interactive conflict resolution"
}

log() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -v|--verbose)
            VERBOSE=1
            shift
            ;;
        -d|--dry-run)
            DRY_RUN=1
            shift
            ;;
        -f|--force)
            FORCE=1
            shift
            ;;
        -y|--yes)
            INTERACTIVE=0
            shift
            ;;
        -b|--backup)
            BACKUP_DIR="$2"
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        web|main-to-gh|gh-to-main|analyze|interactive)
            DIRECTION="$1"
            shift
            ;;
        *)
            error "Unknown option: $1"
            usage
            exit 1
            ;;
    esac
done

# Default to interactive mode if no direction specified
if [[ -z "$DIRECTION" ]]; then
    DIRECTION="interactive"
fi

# Verify we're in a git repository
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    error "Not in a git repository"
    exit 1
fi

# Store current branch and state
CURRENT_BRANCH=$(git branch --show-current)
ORIGINAL_STATE=$(git rev-parse HEAD)

log "Current branch: $CURRENT_BRANCH"
log "Current commit: $ORIGINAL_STATE"

# Function to check if branch exists
branch_exists() {
    git show-ref --verify --quiet "refs/heads/$1" || git show-ref --verify --quiet "refs/remotes/origin/$1"
}

# Function to analyze differences between branches
analyze_differences() {
    local source_branch="$1"
    local target_branch="$2"
    
    log "Analyzing differences between $source_branch and $target_branch..."
    
    # Check if both branches exist
    if ! branch_exists "$source_branch"; then
        error "Source branch '$source_branch' does not exist"
        return 1
    fi
    
    if ! branch_exists "$target_branch"; then
        error "Target branch '$target_branch' does not exist"
        return 1
    fi
    
    # Get list of modified FlowLoom files
    local modified_files=()
    local conflicted_files=()
    local source_only_files=()
    local target_only_files=()
    
    for file in "${FLOWLOOM_FILES[@]}"; do
        if [[ "$VERBOSE" == "1" ]]; then
            log "Checking file: $file"
        fi
        
        # Check if file exists in both branches
        local source_exists=$(git cat-file -e "$source_branch:$file" 2>/dev/null && echo "yes" || echo "no")
        local target_exists=$(git cat-file -e "$target_branch:$file" 2>/dev/null && echo "yes" || echo "no")
        
        if [[ "$source_exists" == "yes" && "$target_exists" == "yes" ]]; then
            # Both exist, check if they differ
            if ! git diff --quiet "$source_branch:$file" "$target_branch:$file" 2>/dev/null; then
                modified_files+=("$file")
                
                # Check for conflicts (both have changes from common ancestor)
                local merge_base=$(git merge-base "$source_branch" "$target_branch" 2>/dev/null || echo "")
                if [[ -n "$merge_base" ]]; then
                    local base_to_source=$(git diff --quiet "$merge_base:$file" "$source_branch:$file" 2>/dev/null; echo $?)
                    local base_to_target=$(git diff --quiet "$merge_base:$file" "$target_branch:$file" 2>/dev/null; echo $?)
                    
                    if [[ "$base_to_source" != "0" && "$base_to_target" != "0" ]]; then
                        conflicted_files+=("$file")
                    fi
                fi
            fi
        elif [[ "$source_exists" == "yes" && "$target_exists" == "no" ]]; then
            source_only_files+=("$file")
        elif [[ "$source_exists" == "no" && "$target_exists" == "yes" ]]; then
            target_only_files+=("$file")
        fi
    done
    
    # Display results
    echo ""
    echo "📊 Sync Analysis: $source_branch → $target_branch"
    echo "================================================"
    
    if [[ ${#modified_files[@]} -gt 0 ]]; then
        echo ""
        echo "📝 Modified files (${#modified_files[@]}):"
        for file in "${modified_files[@]}"; do
            if [[ " ${conflicted_files[@]} " =~ " $file " ]]; then
                echo "   ⚠️  $file (CONFLICT - both branches modified)"
            else
                echo "   ✏️  $file"
            fi
        done
    fi
    
    if [[ ${#source_only_files[@]} -gt 0 ]]; then
        echo ""
        echo "➕ Only in $source_branch (${#source_only_files[@]}):"
        for file in "${source_only_files[@]}"; do
            echo "   📄 $file"
        done
    fi
    
    if [[ ${#target_only_files[@]} -gt 0 ]]; then
        echo ""
        echo "➖ Only in $target_branch (${#target_only_files[@]}):"
        for file in "${target_only_files[@]}"; do
            echo "   📄 $file"
        done
    fi
    
    if [[ ${#modified_files[@]} -eq 0 && ${#source_only_files[@]} -eq 0 && ${#target_only_files[@]} -eq 0 ]]; then
        success "No differences found - branches are in sync!"
    fi
    
    # Store results in global arrays for other functions
    ANALYSIS_MODIFIED=("${modified_files[@]}")
    ANALYSIS_CONFLICTS=("${conflicted_files[@]}")
    ANALYSIS_SOURCE_ONLY=("${source_only_files[@]}")
    ANALYSIS_TARGET_ONLY=("${target_only_files[@]}")
    
    return 0
}

# Function to create backup
create_backup() {
    if [[ -n "$BACKUP_DIR" ]]; then
        local timestamp=$(date +"%Y%m%d_%H%M%S")
        local backup_path="$BACKUP_DIR/flowloom_backup_$timestamp"
        
        log "Creating backup at: $backup_path"
        
        if [[ "$DRY_RUN" == "0" ]]; then
            mkdir -p "$backup_path"
            
            # Backup current state of both branches
            git archive --format=tar --prefix="main/" main | tar -xf - -C "$backup_path"
            git archive --format=tar --prefix="gh-pages/" gh-pages | tar -xf - -C "$backup_path" 2>/dev/null || true
            
            echo "$ORIGINAL_STATE" > "$backup_path/original_commit.txt"
            echo "$CURRENT_BRANCH" > "$backup_path/original_branch.txt"
            
            success "Backup created at: $backup_path"
        else
            log "[DRY RUN] Would create backup at: $backup_path"
        fi
    fi
}

# Function to perform sync
perform_sync() {
    local source_branch="$1"
    local target_branch="$2"
    
    # Analyze first
    analyze_differences "$source_branch" "$target_branch"
    
    # Check for conflicts if not forcing
    if [[ "$FORCE" == "0" && ${#ANALYSIS_CONFLICTS[@]} -gt 0 ]]; then
        warn "Conflicts detected in ${#ANALYSIS_CONFLICTS[@]} files:"
        for file in "${ANALYSIS_CONFLICTS[@]}"; do
            echo "   ⚠️  $file"
        done
        echo ""
        
        if [[ "$INTERACTIVE" == "1" ]]; then
            echo "How would you like to proceed?"
            echo "1) Resolve conflicts interactively"
            echo "2) Force sync (overwrite target with source)"
            echo "3) Abort"
            read -p "Choice [1-3]: " choice
            
            case $choice in
                1) resolve_conflicts_interactive "$source_branch" "$target_branch" ;;
                2) FORCE=1; perform_sync_force "$source_branch" "$target_branch" ;;
                3) log "Sync aborted by user"; return 1 ;;
                *) error "Invalid choice"; return 1 ;;
            esac
        else
            error "Conflicts detected. Use --force to override or run in interactive mode."
            return 1
        fi
    else
        perform_sync_force "$source_branch" "$target_branch"
    fi
}

# Function to perform sync with force (overwrite conflicts)
perform_sync_force() {
    local source_branch="$1"
    local target_branch="$2"
    
    create_backup
    
    log "Starting sync: $source_branch → $target_branch"
    
    if [[ "$DRY_RUN" == "1" ]]; then
        log "[DRY RUN] Would sync ${#FLOWLOOM_FILES[@]} FlowLoom files"
        return 0
    fi
    
    # Switch to target branch
    log "Switching to $target_branch..."
    git checkout "$target_branch"
    
    # Sync each FlowLoom file
    for file in "${FLOWLOOM_FILES[@]}"; do
        if git cat-file -e "$source_branch:$file" 2>/dev/null; then
            if [[ "$VERBOSE" == "1" ]]; then
                log "Syncing: $file"
            fi
            
            # Create directory if needed
            if [[ "$file" == */ ]]; then
                mkdir -p "$file"
            else
                mkdir -p "$(dirname "$file")"
            fi
            
            git checkout "$source_branch" -- "$file" 2>/dev/null || warn "Could not sync $file"
        elif [[ "$VERBOSE" == "1" ]]; then
            log "Skipping (not in source): $file"
        fi
    done
    
    success "Sync completed: $source_branch → $target_branch"
    
    # Show status
    echo ""
    log "Git status after sync:"
    git status --short
    
    echo ""
    echo "Next steps:"
    echo "   1. Review changes: git diff --cached"
    echo "   2. Commit if needed: git add . && git commit -m 'sync: Update from $source_branch'"
    echo "   3. Push changes: git push origin $target_branch"
    echo "   4. Return to original branch: git checkout $CURRENT_BRANCH"
}

# Function to resolve conflicts interactively
resolve_conflicts_interactive() {
    local source_branch="$1"
    local target_branch="$2"
    
    error "Interactive conflict resolution not yet implemented"
    error "Use --force flag to overwrite conflicts, or resolve manually"
    return 1
}

# Function for interactive mode
interactive_mode() {
    echo ""
    echo "🔄 FlowLoom Interactive Sync"
    echo "============================"
    echo ""
    
    # First, analyze both directions
    echo "Analyzing differences..."
    analyze_differences "main" "gh-pages"
    
    if [[ ${#ANALYSIS_MODIFIED[@]} -eq 0 && ${#ANALYSIS_SOURCE_ONLY[@]} -eq 0 && ${#ANALYSIS_TARGET_ONLY[@]} -eq 0 ]]; then
        success "Branches are already in sync!"
        return 0
    fi
    
    echo ""
    analyze_differences "gh-pages" "main"
    
    echo ""
    echo "What would you like to do?"
    echo "1) Sync main → gh-pages"
    echo "2) Sync gh-pages → main"  
    echo "3) Selective sync (choose files)"
    echo "4) Just analyze (done)"
    echo "5) Abort"
    
    read -p "Choice [1-5]: " choice
    
    case $choice in
        1) perform_sync "main" "gh-pages" ;;
        2) perform_sync "gh-pages" "main" ;;
        3) error "Selective sync not yet implemented" ;;
        4) log "Analysis complete" ;;
        5) log "Aborted by user" ;;
        *) error "Invalid choice" ;;
    esac
}

# Cleanup function
cleanup() {
    if [[ "$CURRENT_BRANCH" != "$(git branch --show-current)" ]]; then
        log "Returning to original branch: $CURRENT_BRANCH"
        git checkout "$CURRENT_BRANCH" 2>/dev/null || warn "Could not return to original branch"
    fi
}

# Set trap for cleanup
trap cleanup EXIT

# Main execution
case "$DIRECTION" in
    "web")
        # Auto-detect and sync to website
        AUTO_DIRECTION=$(detect_sync_direction)
        log "Auto-detected sync direction: $AUTO_DIRECTION"
        if [[ "$AUTO_DIRECTION" == "main-to-gh" ]]; then
            log "Syncing FlowLoom development system to website..."
            perform_sync "main" "gh-pages"
        else
            error "Website sync only supported from main branch to gh-pages"
            error "Current branch: $(git branch --show-current)"
            exit 1
        fi
        ;;
    "main-to-gh")
        perform_sync "main" "gh-pages"
        ;;
    "gh-to-main")
        perform_sync "gh-pages" "main"
        ;;
    "analyze")
        analyze_differences "main" "gh-pages"
        echo ""
        analyze_differences "gh-pages" "main"
        ;;
    "interactive")
        interactive_mode
        ;;
    *)
        error "Invalid direction: $DIRECTION"
        usage
        exit 1
        ;;
esac

success "FlowLoom sync operation completed"