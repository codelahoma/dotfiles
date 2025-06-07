#!/usr/bin/env bash
# FlowLoom Installation Checker
# Examines a repo for FlowLoom installation evidence without modifying anything

set -uo pipefail  # Remove -e so script continues on errors

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Check directory
TARGET_DIR="${1:-$(pwd)}"
cd "$TARGET_DIR"

echo -e "${BLUE}FlowLoom Installation Check${NC}"
echo "Directory: $(pwd)"
echo "=================================================="

ISSUES=0
WARNINGS=0

# Function to check file existence and basic format
check_file() {
    local file="$1"
    local description="$2"
    local format_check="${3:-}"
    local required="${4:-true}"
    
    if [[ -f "$file" ]]; then
        echo -e "${GREEN}✓${NC} $description: $file"
        
        # Optional format validation
        if [[ -n "$format_check" ]]; then
            case "$format_check" in
                "json")
                    if ! command -v jq >/dev/null 2>&1; then
                        echo -e "${YELLOW}  ⚠ Cannot validate JSON (jq not installed)${NC}"
                        ((WARNINGS++))
                    elif ! jq empty "$file" 2>/dev/null; then
                        echo -e "${RED}  ✗ Invalid JSON format${NC}"
                        ((ISSUES++))
                    else
                        echo -e "    Valid JSON format"
                    fi
                    ;;
                "markdown")
                    if [[ ! -s "$file" ]]; then
                        echo -e "${YELLOW}  ⚠ File is empty${NC}"
                        ((WARNINGS++))
                    else
                        local lines=$(wc -l < "$file" 2>/dev/null | xargs)
                        echo -e "    $lines lines"
                    fi
                    ;;
            esac
        fi
    else
        if [[ "$required" == "true" ]]; then
            echo -e "${RED}✗${NC} $description: $file (missing)"
            ((ISSUES++))
        else
            echo -e "${YELLOW}⚠${NC} $description: $file (optional, not found)"
            ((WARNINGS++))
        fi
    fi
}

# Function to check directory existence
check_dir() {
    local dir="$1"
    local description="$2"
    local required="${3:-true}"
    
    if [[ -d "$dir" ]]; then
        echo -e "${GREEN}✓${NC} $description: $dir"
        
        # Count contents
        local file_count=$(find "$dir" -type f 2>/dev/null | wc -l | xargs)
        local dir_count=$(find "$dir" -type d 2>/dev/null | wc -l | xargs)
        ((dir_count--)) # Subtract the directory itself
        echo -e "    Files: $file_count, Subdirs: $dir_count"
    else
        if [[ "$required" == "true" ]]; then
            echo -e "${RED}✗${NC} $description: $dir (missing)"
            ((ISSUES++))
        else
            echo -e "${YELLOW}⚠${NC} $description: $dir (optional, not found)"
            ((WARNINGS++))
        fi
    fi
}

# Function to check for file in multiple locations
check_file_locations() {
    local filename="$1"
    local description="$2"
    local format_check="${3:-}"
    local locations=("${@:4}")
    
    local found=false
    local found_location=""
    
    for location in "${locations[@]}"; do
        local full_path="$location/$filename"
        if [[ "$location" == "." ]]; then
            full_path="$filename"
        fi
        
        if [[ -f "$full_path" ]]; then
            found=true
            found_location="$full_path"
            break
        fi
    done
    
    if [[ "$found" == "true" ]]; then
        check_file "$found_location" "$description" "$format_check" "true"
    else
        echo -e "${RED}✗${NC} $description: $filename (not found in: ${locations[*]})"
        ((ISSUES++))
    fi
}

echo -e "\n${BLUE}Essential Claude Code Integration:${NC}"
check_file ".mcp.json" "MCP Configuration (Claude Code)" "json"
check_dir ".claude" "Claude Commands Directory"

echo -e "\n${BLUE}Claude Configuration Files:${NC}"
check_file "CLAUDE.md" "Base Claude Instructions" "markdown"
check_file "CLAUDE.local.md" "Local Claude Configuration" "markdown"

echo -e "\n${BLUE}FlowLoom Installation:${NC}"
check_dir ".flowloom" "FlowLoom Installation Directory"

# Check for installation metadata
if [[ -d ".flowloom" ]]; then
    check_file ".flowloom/installation.json" "Installation Metadata" "json" "false"
    check_file ".flowloom/settings.local.json" "FlowLoom Settings" "json" "false"
    
    # Check if .mcp.json was incorrectly placed in .flowloom
    if [[ -f ".flowloom/.mcp.json" && ! -f ".mcp.json" ]]; then
        echo -e "${YELLOW}⚠${NC} Found .mcp.json in .flowloom/ but it should be in project root"
        ((WARNINGS++))
    fi
fi

echo -e "\n${BLUE}Memory System:${NC}"
check_dir ".knowledge" "Knowledge Directory" "false"
check_file "memory.json" "Memory Configuration" "json" "false"

echo -e "\n${BLUE}Settings:${NC}"
check_file_locations "settings.local.json" "Local Settings" "json" "." ".flowloom"

# Check .mcp.json structure if it exists
if [[ -f ".mcp.json" ]]; then
    echo -e "\n${BLUE}MCP Server Configuration:${NC}"
    
    if command -v jq >/dev/null 2>&1; then
        # Check for mcpServers section
        if jq -e '.mcpServers' .mcp.json >/dev/null 2>&1; then
            echo -e "${GREEN}✓${NC} mcpServers section found"
            
            # Check for required MCP servers
            local servers=(
                "filesystem"
                "memory" 
                "basic-memory"
            )
            
            for server in "${servers[@]}"; do
                if jq -e ".mcpServers.\"$server\"" .mcp.json >/dev/null 2>&1; then
                    echo -e "${GREEN}✓${NC} MCP Server: $server"
                    
                    # Check if server is enabled
                    local enabled=$(jq -r ".mcpServers.\"$server\".enabled // true" .mcp.json 2>/dev/null)
                    if [[ "$enabled" == "false" ]]; then
                        echo -e "    ${YELLOW}⚠ Server is disabled${NC}"
                        ((WARNINGS++))
                    fi
                else
                    echo -e "${YELLOW}⚠${NC} MCP Server: $server (not configured)"
                    ((WARNINGS++))
                fi
            done
        else
            echo -e "${RED}✗${NC} No mcpServers section found in .mcp.json"
            ((ISSUES++))
        fi
    else
        echo -e "${YELLOW}⚠${NC} Cannot validate MCP configuration (jq not installed)"
        ((WARNINGS++))
    fi
fi

# Check .claude commands if directory exists
if [[ -d ".claude" ]]; then
    echo -e "\n${BLUE}Claude Commands:${NC}"
    
    command_count=$(find .claude -name "*.md" -type f 2>/dev/null | wc -l | xargs)
    echo -e "  Total command files: $command_count"
    
    # Check for commands directory structure
    if [[ -d ".claude/commands" ]]; then
        commands_count=$(find .claude/commands -name "*.md" -type f 2>/dev/null | wc -l | xargs)
        echo -e "  Commands in /commands/: $commands_count"
    else
        echo -e "${YELLOW}⚠${NC} No .claude/commands/ directory found"
        ((WARNINGS++))
    fi
    
    # Look for essential commands
    essential_commands=(
        "commands/flowloom.md"
        "commands/sync.md"
    )
    
    optional_commands=(
        "commands/project.md"
        "commands/memory.md"
    )
    
    for cmd in "${essential_commands[@]}"; do
        if [[ -f ".claude/$cmd" ]]; then
            echo -e "${GREEN}✓${NC} Essential command: $cmd"
        else
            echo -e "${RED}✗${NC} Essential command: $cmd (missing)"
            ((ISSUES++))
        fi
    done
    
    for cmd in "${optional_commands[@]}"; do
        if [[ -f ".claude/$cmd" ]]; then
            echo -e "${GREEN}✓${NC} Optional command: $cmd"
        else
            echo -e "${YELLOW}⚠${NC} Optional command: $cmd (not found)"
            ((WARNINGS++))
        fi
    done
fi

# Final Summary
echo -e "\n${BLUE}Installation Summary:${NC}"
echo "=================================================="

if [[ $ISSUES -eq 0 && $WARNINGS -eq 0 ]]; then
    echo -e "${GREEN}✓ Perfect FlowLoom installation - all components found${NC}"
    echo -e "${GREEN}✓ Ready to use with Claude Code${NC}"
    exit 0
elif [[ $ISSUES -eq 0 ]]; then
    echo -e "${YELLOW}⚠ FlowLoom installation mostly complete with $WARNINGS warnings${NC}"
    echo -e "${GREEN}✓ Should work with Claude Code${NC}"
    exit 0
else
    echo -e "${RED}✗ FlowLoom installation incomplete - $ISSUES critical issues, $WARNINGS warnings${NC}"
    echo -e "${RED}✗ May not work properly with Claude Code${NC}"
    
    echo -e "\n${BLUE}Troubleshooting:${NC}"
    echo "1. Re-run the installer: ../flowloom/install-flowloom.py install --profile developer"
    echo "2. Check installer logs for errors"
    echo "3. Ensure you have proper permissions in this directory"
    
    exit 1
fi