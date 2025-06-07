---
title: Standardized MCP Operation Templates
type: note
permalink: development/standardized-mcp-operation-templates
---

# Standardized MCP Operation Templates

## Overview
Reusable templates for creating permission-free coordination commands that follow FlowLoom's Permission Prompt Avoidance policy and universal Shell_ID tagging standards.

## Template 1: Basic Command Structure

### Command Header Template
```markdown
# [Command Name]

[Description] using permission-free MCP operations with proper Shell_ID tagging.

## Usage:
```
/[command] [arguments]
```

Where:
- argument1: Description of argument
- argument2: Description of argument (optional, default: value)

## Permission-Free Implementation
Uses direct MCP memory operations and cached identifiers, following FlowLoom's Permission Prompt Avoidance policy.
```

### Argument Processing Template
```bash
@bash
# Parse arguments and set defaults
arg1="$1"
arg2="${2:-default_value}"
timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
session_id="85299"  # Use cached session ID

# Validate required arguments
if [ -z "$arg1" ]; then
  echo "❌ Error: [Required argument] required"
  echo "Usage: /[command] [arguments]"
  echo ""
  echo "Example: /[command] example_value"
  exit 1
fi

echo "🚀 [Command Name] (Permission-Free)"
echo "====================================="
echo ""
echo "📋 Operation Details:"
echo "   Argument 1: $arg1"
echo "   Argument 2: $arg2"
echo "   Timestamp: $timestamp"
echo "   Session: $session_id"
echo ""
@
```

### MCP Memory Operation Template
```bash
@memory
# Create/update entity with proper Shell_ID tagging
mcp__memory__create_entities '{
  "entities": [
    {
      "name": "[Entity Name]",
      "entityType": "[EntityType]",
      "observations": [
        "Shell_ID: 85299 - '${timestamp}' | [Operation description]",
        "Property 1: '${arg1}'",
        "Property 2: '${arg2}'",
        "Status: active",
        "Created: '${timestamp}'",
        "Method: Permission-free MCP operations"
      ]
    }
  ]
}'

# Update related entities if needed
mcp__memory__add_observations '{
  "observations": [
    {
      "entityName": "[Related Entity]",
      "contents": [
        "Shell_ID: 85299 - '${timestamp}' | [Related operation description]",
        "Updated Property: new_value",
        "Last Modified: '${timestamp}'"
      ]
    }
  ]
}'
@
```

### Success Display Template
```bash
@bash
echo "✅ [Operation] Complete"
echo ""
echo "📊 Operation Summary:"
echo "   [Key Result 1]: $result1"
echo "   [Key Result 2]: $result2"
echo "   Status: Success"
echo "   Method: Permission-free"
echo ""
echo "💡 Next Actions:"
echo "   /[related-command] - [Description]"
echo "   /[status-command] - Check current state"
echo "   /[help-command] - Get additional help"
echo ""
echo "✅ [Operation] completed via permission-free operations!"
@
```

## Template 2: Entity Management Commands

### Create Entity Template
```bash
@memory
# Create new entity with comprehensive tracking
entity_id="[prefix]_$(date +%s)_${session_id}"

mcp__memory__create_entities '{
  "entities": [
    {
      "name": "[Entity Type] '${entity_id}'",
      "entityType": "[EntityType]",
      "observations": [
        "Shell_ID: '${session_id}' - '${timestamp}' | Entity created via permission-free command",
        "Entity ID: '${entity_id}'",
        "Description: '${description}'",
        "Status: '${status}'",
        "Session: '${session_id}'",
        "Created: '${timestamp}'",
        "Creator: controller_'${session_id}'",
        "Creation Method: Permission-free MCP operations"
      ]
    }
  ]
}'

# Create relationships if needed
mcp__memory__create_relations '{
  "relations": [
    {
      "from": "[Parent Entity] '${session_id}'",
      "to": "[Entity Type] '${entity_id}'",
      "relationType": "[relationship_type]"
    }
  ]
}'
@
```

### Update Entity Template
```bash
@memory
# Update existing entity with Shell_ID tagging
mcp__memory__add_observations '{
  "observations": [
    {
      "entityName": "[Entity Name]",
      "contents": [
        "Shell_ID: '${session_id}' - '${timestamp}' | [Update description]",
        "Updated Property: '${new_value}'",
        "Previous Value: '${old_value}'",
        "Update Reason: '${reason}'",
        "Updated By: controller_'${session_id}'",
        "Last Modified: '${timestamp}'"
      ]
    }
  ]
}'
@
```

### Query Entity Template
```bash
@memory
# Query entities with error handling
entity_data=$(mcp__memory__search_nodes '{
  "query": "[Search Query]"
}')

if [ -n "$entity_data" ]; then
  echo "📋 Found [Entity Type]:"
  # Process and display entity data
  echo "   [Property 1]: [Value]"
  echo "   [Property 2]: [Value]"
  echo "   Status: Active"
else
  echo "❌ No [Entity Type] found matching criteria"
  echo "Available options:"
  # Show alternatives
  mcp__memory__search_nodes '{
    "query": "[Alternative Query]"
  }' | grep -E "[Pattern]" | sed 's/^/   /'
fi
@
```

## Template 3: Status Display Commands

### Comprehensive Status Template
```bash
@bash
echo "🔍 [System] Status (Permission-Free)"
echo "================================================"
echo ""
@

@memory
# Query main system status
system_data=$(mcp__memory__search_nodes '{
  "query": "[Main Entity] '${session_id}'"
}')

echo "📋 [System]: ${session_id}"
echo "   Status: Active"
echo "   Role: [Role]"
echo "   Created: [Timestamp]"
echo "   Capabilities: [List]"
echo ""
@

@memory
# Query related entities
related_data=$(mcp__memory__search_nodes '{
  "query": "Session: '${session_id}'"
}')

echo "🔗 Related Components:"
echo "   [Component 1] - [Status]"
echo "      [Details]"
echo ""
echo "   [Component 2] - [Status]"
echo "      [Details]"
echo ""
@

@bash
echo "📊 System Health:"
echo "   ✅ [Health Check 1]"
echo "   ✅ [Health Check 2]"
echo "   ✅ Using permission-free patterns"
echo ""
echo "💡 Available Actions:"
echo "   /[action1] - [Description]"
echo "   /[action2] - [Description]"
echo "   /[action3] - [Description]"
@
```

## Template 4: Error Handling Patterns

### Validation Template
```bash
@bash
# Input validation with helpful error messages
validate_input() {
  local input="$1"
  local type="$2"
  
  case "$type" in
    "required")
      if [ -z "$input" ]; then
        echo "❌ Error: Required parameter missing"
        return 1
      fi
      ;;
    "session_id")
      if ! echo "$input" | grep -q '^[0-9]\+$'; then
        echo "❌ Error: Invalid session ID format"
        return 1
      fi
      ;;
    "priority")
      if ! echo "$input" | grep -q '^(high|medium|low)$'; then
        echo "❌ Error: Priority must be high, medium, or low"
        return 1
      fi
      ;;
  esac
  return 0
}

# Example usage
if ! validate_input "$required_param" "required"; then
  echo "Usage: /[command] [parameters]"
  exit 1
fi
@
```

### Graceful Failure Template
```bash
@memory
# Attempt operation with fallback
operation_result=$(mcp__memory__search_nodes '{
  "query": "[Primary Query]"
}' 2>/dev/null)

if [ -z "$operation_result" ]; then
  echo "⚠️  Primary operation unavailable, trying fallback..."
  
  fallback_result=$(mcp__memory__search_nodes '{
    "query": "[Fallback Query]"
  }' 2>/dev/null)
  
  if [ -n "$fallback_result" ]; then
    echo "✅ Fallback successful"
    # Process fallback result
  else
    echo "❌ Both primary and fallback operations failed"
    echo "Please check system status and try again"
    exit 1
  fi
fi
@
```

## Template 5: Universal Shell_ID Tagging

### Standard Tagging Format
```bash
# All memory operations must include Shell_ID tagging
"Shell_ID: $session_id - $timestamp | [Action description]"

# Examples:
"Shell_ID: 85299 - 2025-05-28T19:20:00Z | Task created via permission-free dispatch"
"Shell_ID: 85299 - 2025-05-28T19:20:00Z | Worker status updated to busy"
"Shell_ID: 85299 - 2025-05-28T19:20:00Z | Coordination session health check performed"
```

### Timestamp Generation
```bash
# Standard UTC timestamp generation
timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Use in observations:
"Created: '${timestamp}'"
"Last Modified: '${timestamp}'"
"Shell_ID: 85299 - '${timestamp}' | [Description]"
```

## Template 6: Command Documentation

### Standard Documentation Format
```markdown
# [Command Name]

[Brief description] using permission-free MCP operations with proper Shell_ID tagging.

## Usage:
```
/[command] [arguments]
```

## Arguments:
- **argument1**: Description and type
- **argument2**: Description and type (optional, default: value)

## Permission-Free Implementation
This command follows FlowLoom's Permission Prompt Avoidance policy by:
- Using cached session IDs instead of shell PID detection
- Direct MCP memory operations instead of bash commands
- Universal Shell_ID tagging for session correlation
- Enhanced error handling and user feedback

## Examples:
```
/[command] example1
/[command] example1 example2
```

## Related Commands:
- `/[related1]` - Description
- `/[related2]` - Description

## See Also:
- Permission Prompt Avoidance documentation
- Universal Shell_ID tagging standards
```

## Implementation Guidelines

### 1. Always Use Templates
- Start with appropriate template for command type
- Customize for specific functionality
- Maintain consistent structure and formatting

### 2. Universal Requirements
- **Shell_ID tagging**: All memory operations must include session correlation
- **Timestamp precision**: Use UTC ISO 8601 format
- **Error handling**: Graceful failures with helpful messages
- **User feedback**: Clear success/failure indicators

### 3. Performance Optimization
- **Cache identifiers**: Avoid repeated shell PID calls
- **Direct MCP operations**: Minimize bash command usage
- **Efficient queries**: Use specific search patterns

### 4. Testing Standards
- **Permission verification**: Ensure no prompts triggered
- **Memory consistency**: Verify Shell_ID tagging
- **Error scenarios**: Test validation and failure cases
- **User experience**: Confirm clear feedback and guidance

## Future Enhancements

### Template Automation
- Command generator script using templates
- Automated Shell_ID tagging insertion
- Template validation and consistency checking

### Extended Patterns
- Multi-entity transaction templates
- Complex workflow coordination templates
- Integration testing templates
- Performance monitoring templates

These templates ensure consistent, permission-free command development across the FlowLoom ecosystem while maintaining professional user experience and comprehensive session tracking.