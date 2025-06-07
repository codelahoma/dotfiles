#!/bin/bash
# safe-switch.sh - Safe git branch switching with memory file handling
# Prevents merge conflicts in fl-memory.json and other memory files

set -e  # Exit on error

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to check if file has changes
has_changes() {
    local file="$1"
    git diff --quiet "$file" 2>/dev/null && git diff --cached --quiet "$file" 2>/dev/null
    return $?
}

# Function to commit memory files
commit_memory_files() {
    local files_to_commit=()
    
    # Check each memory file
    for file in fl-memory.json memory.json sessions/*/memory.json sessions/*/fl-memory.json; do
        if [ -f "$file" ] && ! has_changes "$file"; then
            files_to_commit+=("$file")
            echo -e "${YELLOW}Found changes in: $file${NC}"
        fi
    done
    
    if [ ${#files_to_commit[@]} -gt 0 ]; then
        echo -e "${YELLOW}Committing memory files before switch...${NC}"
        git add "${files_to_commit[@]}"
        git commit -m "chore: auto-commit memory files before branch switch

Automated commit to prevent merge conflicts during branch switch.
Files: ${files_to_commit[*]}"
        echo -e "${GREEN}Memory files committed successfully${NC}"
        return 0
    fi
    return 1
}

# Function to pause auto-tracking
pause_auto_tracking() {
    # Create a marker file to pause auto-tracking
    touch ~/.flowloom-auto-track-paused
    echo -e "${YELLOW}Auto-tracking paused${NC}"
}

# Function to resume auto-tracking
resume_auto_tracking() {
    # Remove the marker file to resume auto-tracking
    rm -f ~/.flowloom-auto-track-paused
    echo -e "${GREEN}Auto-tracking resumed${NC}"
}

# Main script
main() {
    if [ $# -eq 0 ]; then
        echo "Usage: $0 <branch-name> [git checkout options]"
        echo "Example: $0 main"
        echo "Example: $0 -b feature-branch"
        exit 1
    fi
    
    local target_branch="$1"
    shift  # Remove first argument
    
    echo -e "${GREEN}=== Safe Branch Switch ===${NC}"
    echo "Target: $target_branch"
    
    # Step 1: Check for memory file changes
    echo -e "\n${YELLOW}Checking memory files...${NC}"
    if commit_memory_files; then
        echo "Memory files committed"
    else
        echo "No memory file changes to commit"
    fi
    
    # Step 2: Check for other uncommitted changes
    if ! git diff --quiet || ! git diff --cached --quiet; then
        echo -e "\n${YELLOW}You have uncommitted changes:${NC}"
        git status --short
        echo -e "\n${RED}Please commit or stash these changes before switching branches${NC}"
        echo "You can use: git stash push -m 'Work in progress'"
        exit 1
    fi
    
    # Step 3: Pause auto-tracking
    pause_auto_tracking
    
    # Step 4: Perform the branch switch
    echo -e "\n${YELLOW}Switching to branch: $target_branch${NC}"
    if [ "$target_branch" = "-b" ]; then
        # Handle -b flag for creating new branch
        git checkout "$target_branch" "$@"
    else
        git checkout "$target_branch" "$@"
    fi
    
    # Step 5: Resume auto-tracking
    resume_auto_tracking
    
    # Step 6: Show status
    echo -e "\n${GREEN}Successfully switched to:${NC}"
    git branch --show-current
    echo -e "\n${GREEN}Status:${NC}"
    git status --short
    
    echo -e "\n${GREEN}=== Safe switch completed ===${NC}"
}

# Run main function
main "$@"