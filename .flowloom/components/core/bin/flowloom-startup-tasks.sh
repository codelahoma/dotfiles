#!/usr/bin/env bash
# FlowLoom pre-startup tasks
# This file is sourced by the flowloom launcher before starting Claude

set -euo pipefail

# Color codes (inherit from parent if available)
RED="${RED:-\033[0;31m}"
GREEN="${GREEN:-\033[0;32m}"
YELLOW="${YELLOW:-\033[1;33m}"
BLUE="${BLUE:-\033[0;34m}"
NC="${NC:-\033[0m}"

# Function to run a startup task
run_task() {
    local task_name="$1"
    local task_cmd="$2"
    
    echo -ne "  ${task_name}... "
    if eval "$task_cmd" >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC}"
        return 0
    else
        echo -e "${RED}✗${NC}"
        return 1
    fi
}

# Main startup tasks function
run_startup_tasks() {
    local project_dir="${1:-$(pwd)}"
    
    echo -e "${BLUE}Running startup tasks...${NC}"
    
    # 1. Check and update git status
    if [[ -d "$project_dir/.git" ]]; then
        run_task "Git status" "cd '$project_dir' && git fetch --quiet"
    fi
    
    # 2. Ensure required directories exist
    run_task "Project directories" "mkdir -p '$project_dir'/{config,plans,docs,bin}"
    
    # 3. Check memory systems
    if [[ ! -f "$project_dir/memory.json" ]]; then
        run_task "Initialize memory.json" "echo '[]' > '$project_dir/memory.json'"
    fi
    
    # 4. Validate CLAUDE files
    local claude_files_ok=true
    if [[ ! -f "$project_dir/CLAUDE.md" ]]; then
        echo -e "  ${YELLOW}Warning: CLAUDE.md not found${NC}"
        claude_files_ok=false
    fi
    if [[ ! -f "$project_dir/CLAUDE.local.md" ]]; then
        echo -e "  ${YELLOW}Warning: CLAUDE.local.md not found${NC}"
        claude_files_ok=false
    fi
    
    # 5. Check knowledge directory exists (read-only check)
    if [[ -d "$project_dir/.knowledge" ]]; then
        echo -e "  Knowledge directory: ${GREEN}✓${NC}"
    fi
    
    # 6. Check for updates (optional, only if online)
    if command -v curl >/dev/null 2>&1; then
        # Check if we can reach GitHub (timeout quickly)
        if curl -s --connect-timeout 2 https://api.github.com >/dev/null 2>&1; then
            local latest_commit=$(cd "$project_dir" && git ls-remote origin main 2>/dev/null | cut -f1)
            local current_commit=$(cd "$project_dir" && git rev-parse HEAD 2>/dev/null)
            
            if [[ -n "$latest_commit" && -n "$current_commit" && "$latest_commit" != "$current_commit" ]]; then
                echo -e "  ${YELLOW}Note: Updates available (run 'git pull' to update)${NC}"
            fi
        fi
    fi
    
    # 7. Set up shell environment
    run_task "Shell environment" "export FLOWLOOM_PROJECT='$project_dir'"
    
    # 8. Check disk space (warn if low)
    local available_space=$(df -k "$project_dir" | awk 'NR==2 {print $4}')
    if [[ $available_space -lt 1048576 ]]; then  # Less than 1GB
        echo -e "  ${YELLOW}Warning: Low disk space ($(($available_space / 1024))MB available)${NC}"
    fi
    
    # 9. Run custom startup script if exists (from project config dir)
    if [[ -f "$project_dir/config/startup.sh" ]]; then
        echo -e "  Running custom startup script..."
        source "$project_dir/config/startup.sh"
    fi
    
    echo -e "${GREEN}Startup tasks complete${NC}\n"
}

# Export the function so it can be used by the main script
export -f run_startup_tasks