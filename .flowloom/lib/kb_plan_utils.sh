#!/bin/bash
# Knowledge Base Plan Utilities
# Shared functions for plan management using the knowledge base

# Configuration
KB_PLANS_FOLDER="plans"
KB_PLAN_ID_PREFIX="PLAN"

# Generate a unique plan ID
kb_generate_plan_id() {
    local year=$(date +%Y)
    local timestamp=$(date +%s)
    echo "${KB_PLAN_ID_PREFIX}-${year}-${timestamp}"
}

# Write a plan to the knowledge base
# Usage: kb_write_plan "title" "content" "type" "status" "tags" "parent_plan_id"
kb_write_plan() {
    local title="$1"
    local content="$2"
    local type="${3:-implementation}"
    local status="${4:-draft}"
    local tags="${5:-plan}"
    local parent_plan="${6:-}"
    
    local plan_id=$(kb_generate_plan_id)
    local created_date=$(date +%Y-%m-%d)
    
    # Build metadata section
    local metadata="---
plan_id: ${plan_id}
status: ${status}
type: ${type}
created_date: ${created_date}
updated_date: ${created_date}
priority: medium"
    
    if [[ -n "$parent_plan" ]]; then
        metadata="${metadata}
parent_plan: ${parent_plan}"
    fi
    
    metadata="${metadata}
---

"
    
    # Combine metadata and content
    local full_content="${metadata}${content}"
    
    # Write to knowledge base using mcp command
    claude-mcp run basic-memory write_note \
        --title "$title" \
        --content "$full_content" \
        --folder "$KB_PLANS_FOLDER" \
        --tags "$tags"
    
    echo "$plan_id"
}

# Find plans in the knowledge base
# Usage: kb_find_plans "search_query" "status_filter" "type_filter"
kb_find_plans() {
    local query="${1:-}"
    local status_filter="${2:-}"
    local type_filter="${3:-}"
    
    # Build search query
    local search_args="--query \"${query:-plan}\""
    
    if [[ -n "$status_filter" ]]; then
        search_args="${search_args} --query \"status: ${status_filter}\""
    fi
    
    if [[ -n "$type_filter" ]]; then
        search_args="${search_args} --query \"type: ${type_filter}\""
    fi
    
    # Search using mcp command
    eval "claude-mcp run basic-memory search_notes ${search_args}"
}

# Update plan status
# Usage: kb_update_plan_status "plan_id_or_title" "new_status"
kb_update_plan_status() {
    local plan_identifier="$1"
    local new_status="$2"
    local updated_date=$(date +%Y-%m-%d)
    
    # This is a simplified version - in practice, we'd need to:
    # 1. Read the existing plan
    # 2. Parse and update the metadata
    # 3. Write back the updated content
    echo "Updating plan ${plan_identifier} to status: ${new_status}"
    
    # For now, log the update intent
    echo "TODO: Implement full plan status update mechanism"
}

# Get a specific plan
# Usage: kb_get_plan "plan_id_or_title"
kb_get_plan() {
    local plan_identifier="$1"
    
    # Read plan using mcp command
    claude-mcp run basic-memory read_note --identifier "$plan_identifier"
}

# Link two plans
# Usage: kb_link_plans "from_plan_id" "to_plan_id" "relationship_type"
kb_link_plans() {
    local from_plan="$1"
    local to_plan="$2"
    local relationship="${3:-relates-to}"
    
    echo "Linking ${from_plan} -> ${to_plan} (${relationship})"
    
    # This would update the plan metadata to include the relationship
    echo "TODO: Implement plan relationship tracking"
}

# Get recent plans
# Usage: kb_recent_plans "limit"
kb_recent_plans() {
    local limit="${1:-10}"
    
    # Use recent_activity with plans folder filter
    claude-mcp run basic-memory recent_activity \
        --type "plans" \
        --page_size "$limit"
}

# Archive a plan
# Usage: kb_archive_plan "plan_id_or_title"
kb_archive_plan() {
    local plan_identifier="$1"
    
    kb_update_plan_status "$plan_identifier" "archived"
}

# Export functions for use in other scripts
export -f kb_generate_plan_id
export -f kb_write_plan
export -f kb_find_plans
export -f kb_update_plan_status
export -f kb_get_plan
export -f kb_link_plans
export -f kb_recent_plans
export -f kb_archive_plan