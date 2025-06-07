#!/bin/bash
# bin/session_memory.sh - MCP memory integration via python shebang scripts
# Part of FlowLoom Session Management System - Phase 3: Integration Layer
# Based on Plan 3120 - Session Tracking Detailed Design
#
# SECURITY & ETHICS NOTICE:
# This script integrates session management with MCP memory while respecting Claude Code's
# security boundaries. All operations are confined to designated session directories
# and use only authorized MCP memory operations.

set -euo pipefail

# Import session utilities
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
source "$SCRIPT_DIR/session_utils.sh"
source "$SCRIPT_DIR/session_filesystem.sh"

# Memory integration functions

# Tag entity with current session
tag_entity_with_session() {
    local entity_name="$1"
    local session_id="$2"
    local base_dir="${3:-.flowloom}"
    local additional_observations="${4:-}"
    
    session_debug "Tagging entity '$entity_name' with session: $session_id"
    
    if ! validate_session_id "$session_id"; then
        session_error "Invalid session ID: $session_id"
        return 1
    fi
    
    # Get session metadata for context
    local session_dir
    session_dir=$(get_session_directory "$session_id" "$base_dir")
    local metadata_file="$session_dir/metadata.json"
    
    if [[ ! -f "$metadata_file" ]]; then
        session_error "Session metadata not found: $metadata_file"
        return 1
    fi
    
    # Prepare observations using Python MCP client
    python3 << EOF
import json
import sys
from datetime import datetime

# Read session metadata
try:
    with open('$metadata_file', 'r') as f:
        metadata = json.load(f)
except Exception as e:
    print(f"Error reading metadata: {e}", file=sys.stderr)
    sys.exit(1)

# Prepare session tag and observations
session_tag = f"Session_{metadata['sessionId']}"
timestamp = datetime.now().isoformat()

observations = [
    f"Tagged_By_Session: {metadata['sessionId']}",
    f"Session_Timestamp: {timestamp}",
    f"Session_Context: {metadata.get('sessionContext', 'Unknown')}",
    f"Session_Working_Directory: {metadata.get('workingDirectory', 'Unknown')}",
    f"Session_Git_Branch: {metadata.get('gitBranch', 'Unknown')}",
    f"Session_Active_Mode: {metadata.get('activeMode', 'Unknown')}"
]

# Add additional observations if provided
if '$additional_observations':
    additional_list = '$additional_observations'.split(',')
    observations.extend([obs.strip() for obs in additional_list if obs.strip()])

# Output MCP commands for external execution
print("# MCP Memory Operations for Entity Tagging")
print(f"# Entity: $entity_name")
print(f"# Session: {metadata['sessionId']}")
print()

# Create session entity if it doesn't exist
print("# Create session entity")
print(f"mcp_memory_create_entities '{session_tag}' 'Session' 'FlowLoom session tracking entity'")
print()

# Add observations to target entity
print("# Add session observations to target entity")
for obs in observations:
    print(f"mcp_memory_add_observation '$entity_name' '{obs}'")

print()
print("# Create relationship between entity and session")
print(f"mcp_memory_create_relation '$entity_name' '{session_tag}' 'created_in_session'")

print()
print(f"# Tag operation completed for entity: $entity_name")
EOF
    
    if [[ $? -eq 0 ]]; then
        session_log "Generated MCP commands for tagging entity '$entity_name' with session: $session_id"
    else
        session_error "Failed to generate MCP commands for entity tagging"
        return 1
    fi
}

# Get entities created in session
get_session_entities() {
    local session_id="$1"
    local output_format="${2:-list}"  # list, json, summary
    
    session_debug "Getting entities for session: $session_id (format: $output_format)"
    
    if ! validate_session_id "$session_id"; then
        session_error "Invalid session ID: $session_id"
        return 1
    fi
    
    # Generate MCP search command using Python
    python3 << EOF
import sys

search_query = f"Tagged_By_Session: $session_id"

print("# MCP Memory Search for Session Entities")
print(f"# Session: $session_id")
print(f"# Output format: $output_format")
print()

if '$output_format' == 'json':
    print(f"mcp_memory_search_nodes_json '{search_query}'")
elif '$output_format' == 'summary':
    print(f"mcp_memory_search_nodes_summary '{search_query}'")
else:
    print(f"mcp_memory_search_nodes '{search_query}'")

print()
print("# Use the above MCP command to retrieve session entities")
EOF
    
    session_log "Generated MCP search command for session entities: $session_id"
}

# Create session tracking entity in memory
create_session_memory_entity() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    local context_description="${3:-Session tracking entity}"
    
    session_debug "Creating session memory entity: $session_id"
    
    if ! validate_session_id "$session_id"; then
        session_error "Invalid session ID: $session_id"
        return 1
    fi
    
    # Get session metadata
    local session_dir
    session_dir=$(get_session_directory "$session_id" "$base_dir")
    local metadata_file="$session_dir/metadata.json"
    
    if [[ ! -f "$metadata_file" ]]; then
        session_error "Session metadata not found: $metadata_file"
        return 1
    fi
    
    # Create comprehensive session entity using Python
    python3 << EOF
import json
import sys
from datetime import datetime

# Read session metadata
try:
    with open('$metadata_file', 'r') as f:
        metadata = json.load(f)
except Exception as e:
    print(f"Error reading metadata: {e}", file=sys.stderr)
    sys.exit(1)

# Prepare session entity data
entity_name = f"Session_{metadata['sessionId']}"
entity_type = "FlowLoom_Session"

# Create comprehensive observations
observations = [
    f"Session_ID: {metadata['sessionId']}",
    f"PPID: {metadata.get('ppid', 'Unknown')}",
    f"Start_Time: {metadata.get('startTime', 'Unknown')}",
    f"Status: {metadata.get('status', 'Unknown')}",
    f"Working_Directory: {metadata.get('workingDirectory', 'Unknown')}",
    f"Git_Branch: {metadata.get('gitBranch', 'Unknown')}",
    f"Active_Mode: {metadata.get('activeMode', 'Unknown')}",
    f"Context: {metadata.get('sessionContext', 'Unknown')}",
    f"Last_Activity: {metadata.get('lastActivity', 'Unknown')}",
    f"Command_Count: {metadata.get('commandCount', 0)}",
    f"Description: $context_description",
    f"Created_At: {datetime.now().isoformat()}"
]

# Add end time if session is terminated
if metadata.get('endTime'):
    observations.append(f"End_Time: {metadata['endTime']}")

# Add memory entities if present
if 'memoryEntities' in metadata and metadata['memoryEntities']:
    entity_list = ', '.join(metadata['memoryEntities'])
    observations.append(f"Related_Entities: {entity_list}")

# Add todo items if present
if 'todoItems' in metadata and metadata['todoItems']:
    todo_count = len(metadata['todoItems'])
    observations.append(f"Todo_Count: {todo_count}")

# Add artifacts if present
if 'artifacts' in metadata and metadata['artifacts']:
    artifact_count = len(metadata['artifacts'])
    observations.append(f"Artifact_Count: {artifact_count}")

print("# MCP Memory Operations for Session Entity Creation")
print(f"# Session: {metadata['sessionId']}")
print()

print("# Create session entity with comprehensive observations")
print(f"mcp_memory_create_entity '{entity_name}' '{entity_type}' \\")
for i, obs in enumerate(observations):
    separator = " \\\\" if i < len(observations) - 1 else ""
    print(f"  '{obs}'{separator}")

print()
print(f"# Session entity creation completed: {entity_name}")
EOF
    
    if [[ $? -eq 0 ]]; then
        session_log "Generated MCP commands for session entity creation: $session_id"
    else
        session_error "Failed to generate MCP commands for session entity creation"
        return 1
    fi
}

# Update session status in memory
update_session_memory_status() {
    local session_id="$1"
    local new_status="$2"
    local base_dir="${3:-.flowloom}"
    
    session_debug "Updating session memory status: $session_id -> $new_status"
    
    if ! validate_session_id "$session_id"; then
        session_error "Invalid session ID: $session_id"
        return 1
    fi
    
    # Validate status
    case "$new_status" in
        "active"|"inactive"|"terminated") ;;
        *) 
            session_error "Invalid status: $new_status (must be active, inactive, or terminated)"
            return 1
            ;;
    esac
    
    # Generate MCP update commands
    python3 << EOF
from datetime import datetime

entity_name = f"Session_$session_id"
timestamp = datetime.now().isoformat()

print("# MCP Memory Operations for Session Status Update")
print(f"# Session: $session_id")
print(f"# New Status: $new_status")
print()

print("# Update session status observation")
print(f"mcp_memory_add_observation '{entity_name}' 'Status_Updated: $new_status at {timestamp}'")

if '$new_status' == 'terminated':
    print(f"mcp_memory_add_observation '{entity_name}' 'End_Time: {timestamp}'")

print()
print(f"# Session status update completed: $session_id -> $new_status")
EOF
    
    session_log "Generated MCP commands for session status update: $session_id -> $new_status"
}

# Add session progress observation
add_session_progress() {
    local session_id="$1"
    local progress_description="$2"
    local progress_type="${3:-general}"  # general, plan, implementation, decision
    
    session_debug "Adding session progress: $session_id - $progress_description"
    
    if ! validate_session_id "$session_id"; then
        session_error "Invalid session ID: $session_id"
        return 1
    fi
    
    # Generate MCP progress observation
    python3 << EOF
from datetime import datetime

entity_name = f"Session_$session_id"
timestamp = datetime.now().isoformat()

print("# MCP Memory Operations for Session Progress")
print(f"# Session: $session_id")
print(f"# Progress Type: $progress_type")
print()

progress_obs = f"Progress_{timestamp}: [$progress_type] $progress_description"
print(f"mcp_memory_add_observation '{entity_name}' '{progress_obs}'")

print()
print(f"# Session progress added: $session_id")
EOF
    
    session_log "Generated MCP command for session progress: $session_id"
}

# Link session to plan entity
link_session_to_plan() {
    local session_id="$1"
    local plan_entity="$2"
    local relationship_type="${3:-working_on}"
    
    session_debug "Linking session to plan: $session_id -> $plan_entity"
    
    if ! validate_session_id "$session_id"; then
        session_error "Invalid session ID: $session_id"
        return 1
    fi
    
    # Generate MCP relationship command
    python3 << EOF
from datetime import datetime

session_entity = f"Session_$session_id"
timestamp = datetime.now().isoformat()

print("# MCP Memory Operations for Session-Plan Linking")
print(f"# Session: $session_id")
print(f"# Plan: $plan_entity")
print(f"# Relationship: $relationship_type")
print()

print("# Create relationship between session and plan")
print(f"mcp_memory_create_relation '{session_entity}' '$plan_entity' '$relationship_type'")

print()
print("# Add linking observation to session")
print(f"mcp_memory_add_observation '{session_entity}' 'Linked_To_Plan: $plan_entity via $relationship_type at {timestamp}'")

print()
print(f"# Session-plan linking completed: $session_id -> $plan_entity")
EOF
    
    session_log "Generated MCP commands for session-plan linking: $session_id -> $plan_entity"
}

# Export session to memory format
export_session_to_memory() {
    local session_id="$1"
    local base_dir="${2:-.flowloom}"
    local export_format="${3:-commands}"  # commands, json, summary
    
    session_debug "Exporting session to memory format: $session_id (format: $export_format)"
    
    if ! validate_session_id "$session_id"; then
        session_error "Invalid session ID: $session_id"
        return 1
    fi
    
    local session_dir
    session_dir=$(get_session_directory "$session_id" "$base_dir")
    
    if [[ ! -d "$session_dir" ]]; then
        session_error "Session directory not found: $session_dir"
        return 1
    fi
    
    # Export session data using Python
    python3 << EOF
import json
import sys
import os
from datetime import datetime

session_dir = '$session_dir'
export_format = '$export_format'

# Read session files
session_data = {
    'sessionId': '$session_id',
    'exportTime': datetime.now().isoformat(),
    'metadata': None,
    'state': None,
    'artifacts': []
}

# Read metadata
metadata_file = os.path.join(session_dir, 'metadata.json')
if os.path.exists(metadata_file):
    try:
        with open(metadata_file, 'r') as f:
            session_data['metadata'] = json.load(f)
    except Exception as e:
        print(f"Warning: Could not read metadata: {e}", file=sys.stderr)

# Read state
state_file = os.path.join(session_dir, 'state.json')
if os.path.exists(state_file):
    try:
        with open(state_file, 'r') as f:
            session_data['state'] = json.load(f)
    except Exception as e:
        print(f"Warning: Could not read state: {e}", file=sys.stderr)

# List artifacts
artifacts_dir = os.path.join(session_dir, 'artifacts')
if os.path.exists(artifacts_dir):
    try:
        for item in os.listdir(artifacts_dir):
            item_path = os.path.join(artifacts_dir, item)
            if os.path.isfile(item_path):
                stat = os.stat(item_path)
                session_data['artifacts'].append({
                    'name': item,
                    'size': stat.st_size,
                    'modified': datetime.fromtimestamp(stat.st_mtime).isoformat()
                })
    except Exception as e:
        print(f"Warning: Could not list artifacts: {e}", file=sys.stderr)

if export_format == 'json':
    print(json.dumps(session_data, indent=2))
elif export_format == 'summary':
    print(f"Session Export Summary: $session_id")
    print(f"Export Time: {session_data['exportTime']}")
    if session_data['metadata']:
        print(f"Status: {session_data['metadata'].get('status', 'Unknown')}")
        print(f"Context: {session_data['metadata'].get('sessionContext', 'Unknown')}")
        print(f"Last Activity: {session_data['metadata'].get('lastActivity', 'Unknown')}")
    if session_data['state']:
        state_data = session_data['state'].get('data', {})
        print(f"Current Plan: {state_data.get('currentPlan', 'None')}")
        print(f"Active Commands: {len(state_data.get('activeCommands', []))}")
        print(f"Recent Files: {len(state_data.get('recentFiles', []))}")
    print(f"Artifacts: {len(session_data['artifacts'])}")
else:  # commands format
    print("# Complete MCP Memory Export for Session")
    print(f"# Session: $session_id")
    print(f"# Generated: {session_data['exportTime']}")
    print()
    
    if session_data['metadata']:
        entity_name = f"Session_$session_id"
        print("# Create comprehensive session entity")
        print(f"mcp_memory_create_entity '{entity_name}' 'FlowLoom_Session' \\")
        
        metadata = session_data['metadata']
        observations = [
            f"Session_ID: {metadata.get('sessionId', 'Unknown')}",
            f"PPID: {metadata.get('ppid', 'Unknown')}",
            f"Start_Time: {metadata.get('startTime', 'Unknown')}",
            f"Status: {metadata.get('status', 'Unknown')}",
            f"Working_Directory: {metadata.get('workingDirectory', 'Unknown')}",
            f"Git_Branch: {metadata.get('gitBranch', 'Unknown')}",
            f"Active_Mode: {metadata.get('activeMode', 'Unknown')}",
            f"Context: {metadata.get('sessionContext', 'Unknown')}",
            f"Last_Activity: {metadata.get('lastActivity', 'Unknown')}",
            f"Command_Count: {metadata.get('commandCount', 0)}",
            f"Exported_At: {session_data['exportTime']}"
        ]
        
        if metadata.get('endTime'):
            observations.append(f"End_Time: {metadata['endTime']}")
        
        for i, obs in enumerate(observations):
            separator = " \\\\" if i < len(observations) - 1 else ""
            print(f"  '{obs}'{separator}")
        
        print()
        
        # Add state information
        if session_data['state'] and 'data' in session_data['state']:
            state_data = session_data['state']['data']
            print("# Add state observations")
            
            if state_data.get('currentPlan'):
                print(f"mcp_memory_add_observation '{entity_name}' 'Current_Plan: {state_data['currentPlan']}'")
            
            if state_data.get('gitState', {}).get('branch'):
                print(f"mcp_memory_add_observation '{entity_name}' 'Git_Branch: {state_data['gitState']['branch']}'")
            
            if state_data.get('activeCommands'):
                cmd_count = len(state_data['activeCommands'])
                print(f"mcp_memory_add_observation '{entity_name}' 'Active_Commands_Count: {cmd_count}'")
        
        print()
        
        # Add artifact information
        if session_data['artifacts']:
            artifact_count = len(session_data['artifacts'])
            print(f"mcp_memory_add_observation '{entity_name}' 'Artifact_Count: {artifact_count}'")
            
            for artifact in session_data['artifacts'][:5]:  # Limit to first 5
                print(f"mcp_memory_add_observation '{entity_name}' 'Artifact: {artifact['name']} ({artifact['size']} bytes)'")
    
    print()
    print(f"# Session export completed: $session_id")

EOF
    
    if [[ $? -eq 0 ]]; then
        session_log "Exported session to memory format: $session_id (format: $export_format)"
    else
        session_error "Failed to export session to memory format"
        return 1
    fi
}

# Search sessions by criteria
search_sessions_in_memory() {
    local search_criteria="$1"
    local output_format="${2:-list}"  # list, json, summary
    
    session_debug "Searching sessions in memory: $search_criteria"
    
    # Generate MCP search command
    python3 << EOF
print("# MCP Memory Search for Sessions")
print(f"# Criteria: $search_criteria")
print(f"# Output format: $output_format")
print()

if '$output_format' == 'json':
    print(f"mcp_memory_search_nodes_json 'entityType:FlowLoom_Session AND $search_criteria'")
elif '$output_format' == 'summary':
    print(f"mcp_memory_search_nodes_summary 'entityType:FlowLoom_Session AND $search_criteria'")
else:
    print(f"mcp_memory_search_nodes 'entityType:FlowLoom_Session AND $search_criteria'")

print()
print("# Use the above MCP command to search sessions")
EOF
    
    session_log "Generated MCP search command for sessions: $search_criteria"
}

# Print help
print_memory_help() {
    cat << EOF
FlowLoom Session Memory Integration

Usage: session_memory.sh <command> [options]

Commands:
  tag-entity <entity> <session_id> [base_dir] [observations]    Tag entity with session
  get-entities <session_id> [format]                           Get entities for session
  create-entity <session_id> [base_dir] [description]          Create session memory entity
  update-status <session_id> <status> [base_dir]               Update session status in memory
  add-progress <session_id> <description> [type]               Add progress observation
  link-plan <session_id> <plan_entity> [relationship]          Link session to plan
  export <session_id> [base_dir] [format]                      Export session to memory format
  search <criteria> [format]                                   Search sessions in memory

Parameters:
  format: list, json, summary, commands (default varies by command)
  status: active, inactive, terminated
  type: general, plan, implementation, decision (default: general)
  relationship: working_on, implements, related_to (default: working_on)

Examples:
  session_memory.sh tag-entity "Component_A" session_123_456_abc123
  session_memory.sh create-entity session_123_456_abc123
  session_memory.sh update-status session_123_456_abc123 terminated
  session_memory.sh add-progress session_123_456_abc123 "Completed Phase 2" implementation
  session_memory.sh export session_123_456_abc123 .flowloom commands

Output:
  Most commands generate MCP memory operation commands that should be executed
  by an MCP-capable client. Commands are printed to stdout for external execution.

Notes:
  - Integrates with FlowLoom MCP memory system
  - Generates MCP commands for external execution
  - Maintains session-entity relationships
  - Supports comprehensive session tracking
  - Respects Claude Code security boundaries
EOF
}

# Main command dispatcher
main() {
    if [[ $# -eq 0 ]]; then
        print_memory_help
        exit 1
    fi
    
    local command="$1"
    shift
    
    case "$command" in
        "tag-entity")
            if [[ $# -lt 2 ]]; then
                echo "Error: tag-entity requires entity_name and session_id" >&2
                exit 1
            fi
            tag_entity_with_session "$@"
            ;;
        "get-entities")
            if [[ $# -lt 1 ]]; then
                echo "Error: get-entities requires session_id" >&2
                exit 1
            fi
            get_session_entities "$@"
            ;;
        "create-entity")
            if [[ $# -lt 1 ]]; then
                echo "Error: create-entity requires session_id" >&2
                exit 1
            fi
            create_session_memory_entity "$@"
            ;;
        "update-status")
            if [[ $# -lt 2 ]]; then
                echo "Error: update-status requires session_id and status" >&2
                exit 1
            fi
            update_session_memory_status "$@"
            ;;
        "add-progress")
            if [[ $# -lt 2 ]]; then
                echo "Error: add-progress requires session_id and description" >&2
                exit 1
            fi
            add_session_progress "$@"
            ;;
        "link-plan")
            if [[ $# -lt 2 ]]; then
                echo "Error: link-plan requires session_id and plan_entity" >&2
                exit 1
            fi
            link_session_to_plan "$@"
            ;;
        "export")
            if [[ $# -lt 1 ]]; then
                echo "Error: export requires session_id" >&2
                exit 1
            fi
            export_session_to_memory "$@"
            ;;
        "search")
            if [[ $# -lt 1 ]]; then
                echo "Error: search requires criteria" >&2
                exit 1
            fi
            search_sessions_in_memory "$@"
            ;;
        "help"|"--help"|"-h")
            print_memory_help
            ;;
        *)
            echo "Error: Unknown command: $command" >&2
            echo "Use 'session_memory.sh help' for usage information" >&2
            exit 1
            ;;
    esac
}

# Run main function if script is executed directly
if [[ "${BASH_SOURCE[0]:-$0}" == "${0}" ]]; then
    main "$@"
fi