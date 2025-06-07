---
title: Permission-Free Coordination Command Patterns
type: note
permalink: development/permission-free-coordination-command-patterns
---

# Permission-Free Coordination Command Patterns

## Overview
FlowLoom follows a Permission Prompt Avoidance policy. This document outlines patterns for implementing coordination commands without triggering Claude Code permission prompts.

## Core Principle
**Use MCP memory operations instead of bash commands** for coordination state management.

## Pattern: Task Dispatch

### ❌ Permission-Triggering Approach
```bash
# This triggers permission prompts
task_id="task_$(date +%s)_${shell_pid}"
echo "Dispatching task..."
# Complex bash variable manipulation
```

### ✅ Permission-Free Approach
```javascript
// Use direct MCP memory operations
mcp__memory__create_entities({
  "entities": [{
    "name": "Task task_research_async_85299",
    "entityType": "CoordinationTask",
    "observations": [
      "Task ID: task_research_async_85299",
      "Description: Research Python async/await best practices and patterns",
      "Status: pending",
      "Priority: high",
      "Session: 85299"
    ]
  }]
})
```

## Coordination Command Redesign Strategy

### 1. Task Management
- **Create Tasks**: Use `mcp__memory__create_entities` 
- **Assign Tasks**: Use `mcp__memory__add_observations`
- **Update Status**: Use `mcp__memory__add_observations`
- **Cancel Tasks**: Use `mcp__memory__add_observations`

### 2. Worker Management
- **Register Workers**: Use `mcp__memory__create_entities`
- **Update Worker Status**: Use `mcp__memory__add_observations`
- **Worker Heartbeats**: Use `mcp__memory__add_observations`

### 3. Session Management
- **Session State**: Stored in memory graph entities
- **Shell PID Caching**: Use memory observations instead of environment variables
- **Status Reporting**: Query memory graph instead of bash parsing

### 4. Display Operations
- **Status Reports**: Use memory search results formatted in response
- **Task Lists**: Query and format memory entities
- **Worker Dashboards**: Aggregate memory data

## Implementation Guidelines

### Shell PID Handling
```javascript
// Cache shell PID in memory on first use
mcp__memory__add_observations({
  "observations": [{
    "entityName": "Coordination Session 85299",
    "contents": ["Shell PID Cached: 85299", "Avoid repeated get_shell_pid.sh calls"]
  }]
})
```

### Task ID Generation
```javascript
// Use timestamp + session for unique IDs
const taskId = `task_${Date.now()}_${sessionId}`;
// Or use descriptive names: task_research_async_85299
```

### Status Updates
```javascript
// Update entity status without bash commands
mcp__memory__add_observations({
  "observations": [{
    "entityName": "Worker worker_91625",
    "contents": ["Status: Busy", "Current Task: task_research_async_85299"]
  }]
})
```

## Command Redesign Priorities

1. **coord:dispatch** ✅ - Implemented permission-free pattern
2. **coord:status** - Already mostly permission-free
3. **coord:init** - Needs shell PID caching improvement
4. **coord:cancel** - Use memory updates only
5. **worker:announce** - Use memory operations only

## Benefits of Permission-Free Approach

- **Seamless UX**: No interruptions for permission prompts
- **Faster Execution**: Direct MCP operations are more efficient  
- **More Reliable**: No bash command failures or environment issues
- **Better Integration**: Native to Claude Code's MCP architecture
- **Audit Trail**: All coordination state tracked in memory graph

## Testing Strategy

- Test each coordination command without permission prompts
- Verify functionality equivalent to bash-based versions
- Document any limitations or edge cases
- Create fallback patterns for complex operations

## Future Improvements

- **Template System**: Standardized MCP operation patterns
- **Command Generator**: Auto-generate permission-free coordination commands
- **Validation Layer**: Ensure coordination state consistency
- **Error Handling**: Graceful handling of MCP operation failures