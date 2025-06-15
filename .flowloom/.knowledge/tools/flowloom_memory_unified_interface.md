---
type: reference
category: tools
tags:
- memory
- cli
- unified-interface
- wrapper
- primary-tool
status: active
depends_on:
- log_observation.py
- memory-monitor
- fl-memory.json
related_to:
- log_observation_user_guide
- memory_monitor_user_guide
introduced_in: FlowLoom 2.0.0
last_verified: '2025-06-07'
complexity: beginner
stability: stable
priority: critical
frequency: daily
audience:
- developers
- end-users
- contributors
file_paths:
- .flowloom/bin/flowloom-memory
commands:
- flowloom-memory
completeness: 100
needs_update: false
review_date: '2025-07-07'
maintainer: FlowLoom Core Team
permalink: tools/flowloom-memory-unified-interface
---

# FlowLoom Memory - Unified Interface

The complete command-line interface for FlowLoom's memory system, combining the functionality of both `log_observation.py` and `memory-monitor` into a single, cohesive tool.

## Overview

`flowloom-memory` is the recommended way to interact with FlowLoom's knowledge graph. It provides a unified interface that:
- Routes writing operations to `log_observation.py`
- Routes query operations to `memory-monitor`
- Maintains consistent argument patterns and help system
- Simplifies the user experience with a single command to learn

## Installation & Location

```bash
# Script location
/Users/rodk/.homesick/repos/dotfiles/.flowloom/bin/flowloom-memory

# Should be in PATH via FlowLoom installation
flowloom-memory --help
```

## Command Categories

### Writing Commands (routes to log_observation.py)
- `add-entity` - Create new entity or update existing with observation
- `add-observation` - Add observation to existing entity  
- `add-relation` - Create relationship between two entities
- `get-entity` - Retrieve entity details by name
- `list-entities` - List all entities or filter by type

### Query Commands (routes to memory-monitor)
- `query` - Execute SQL query against memory data
- `search` - Quick search for entities
- `watch` - Watch memory file for changes
- `wait-for` - Wait for query condition to be met
- `serve` - Start WebSocket server for real-time updates
- `explain` - Show query execution plan

### Shared Commands
- `stats` - Show memory statistics and summary
- `help` - Show unified help message

## Basic Usage

```bash
# Get help
flowloom-memory --help
flowloom-memory <command> --help

# Writing operations
flowloom-memory add-entity "Authentication System" "Feature" "Implemented OAuth2 flow"
flowloom-memory add-observation "Session-123" "Fixed critical bug in auth validation"
flowloom-memory add-relation "Auth Module" "Database Layer" "depends_on"

# Query operations  
flowloom-memory query "SELECT * FROM entities WHERE type = 'Feature'"
flowloom-memory search "authentication"
flowloom-memory stats

# Real-time monitoring
flowloom-memory watch
flowloom-memory wait-for "SELECT COUNT(*) FROM entities WHERE type = 'Task' >= 10"
```

## Configuration

### Memory File Options

```bash
# Use default memory file (fl-memory.json in current directory)
flowloom-memory add-entity "Test" "Feature" "Description"

# Specify custom memory file
flowloom-memory --memory-file /path/to/custom.json add-entity "Test" "Feature" "Description"

# Use environment variable
export FLOWLOOM_MEMORY_FILE=/path/to/project-memory.json
flowloom-memory add-entity "Test" "Feature" "Description"
```

### Environment Variables

- `FLOWLOOM_MEMORY_FILE` - Default memory file path (default: `fl-memory.json`)

## Examples by Use Case

### Development Session Tracking

```bash
# Start new session
flowloom-memory add-entity "Session-$(date +%s)" "Session" "Started work on authentication feature"

# Log progress
flowloom-memory add-observation "Session-$(date +%s)" "Implemented OAuth2 client registration"
flowloom-memory add-observation "Session-$(date +%s)" "Added JWT token validation"

# Create relationships
flowloom-memory add-relation "Authentication System" "OAuth2 Service" "uses"
flowloom-memory add-relation "Session-$(date +%s)" "Authentication System" "implements"

# Review progress
flowloom-memory query "SELECT * FROM entities WHERE type = 'Session' ORDER BY created_at DESC LIMIT 5"
```

### Architecture Documentation

```bash
# Document components
flowloom-memory add-entity "API Gateway" "Component" "Main entry point for all external requests"
flowloom-memory add-entity "Auth Service" "Component" "Handles authentication and authorization"
flowloom-memory add-entity "Database Layer" "Component" "Data persistence and retrieval"

# Map dependencies
flowloom-memory add-relation "API Gateway" "Auth Service" "depends_on"
flowloom-memory add-relation "Auth Service" "Database Layer" "depends_on"

# Query architecture
flowloom-memory query "SELECT from_entity, to_entity, relation_type FROM relations WHERE relation_type = 'depends_on'"
```

### Feature Development Lifecycle

```bash
# Document feature
flowloom-memory add-entity "User Profile Management" "Feature" "Allow users to manage their profile information"

# Track implementation
flowloom-memory add-observation "User Profile Management" "Created user profile API endpoints"
flowloom-memory add-observation "User Profile Management" "Implemented profile validation logic"
flowloom-memory add-observation "User Profile Management" "Added profile update notifications"

# Link to components
flowloom-memory add-relation "User Profile Management" "API Gateway" "implements"
flowloom-memory add-relation "User Profile Management" "Database Layer" "uses"

# Monitor completion
flowloom-memory query "SELECT content FROM observations WHERE entity_name = 'User Profile Management' ORDER BY timestamp DESC"
```

### Project Monitoring

```bash
# Watch for changes in real-time
flowloom-memory watch

# Wait for milestones
flowloom-memory wait-for "SELECT COUNT(*) FROM entities WHERE type = 'Feature' AND name LIKE '%completed%' >= 5"

# Generate reports
flowloom-memory query "SELECT type, COUNT(*) as count FROM entities GROUP BY type"
flowloom-memory search "authentication"
```

## Advanced Features

### Chained Operations

```bash
# Add entity and immediately query it
flowloom-memory add-entity "New Feature" "Feature" "Initial implementation" && \
flowloom-memory query "SELECT * FROM entities WHERE name = 'New Feature'"

# Bulk relationship creation
for component in "Auth" "Database" "Cache"; do
  flowloom-memory add-relation "API Gateway" "$component Service" "depends_on"
done
```

### Integration with Scripts

```bash
#!/bin/bash
# Development session automation

SESSION_ID="Session-$(date +%s)"
flowloom-memory add-entity "$SESSION_ID" "Session" "Automated session started"

# Your development work here...

flowloom-memory add-observation "$SESSION_ID" "Session completed successfully"
flowloom-memory query "SELECT COUNT(*) FROM observations WHERE entity_name = '$SESSION_ID'"
```

### Pipeline Integration

```bash
# CI/CD integration
echo "Deployment completed successfully" | \
  flowloom-memory add-observation "Production Environment" --from-stdin

# Log analysis
grep "ERROR" /var/log/app.log | \
  head -5 | \
  while read error; do
    flowloom-memory add-observation "Error Tracking" "$error"
  done
```

## Command-Specific Help

Each command provides detailed help when called with `--help`:

```bash
flowloom-memory add-entity --help
flowloom-memory query --help
flowloom-memory search --help
```

## Benefits Over Individual Tools

### Unified Experience
- Single command to learn and remember
- Consistent argument patterns across all operations
- Unified help system and documentation
- Seamless workflow between writing and querying

### Simplified Integration
- One tool to integrate in scripts and automation
- Consistent error handling and output formats
- Single configuration point for memory file location
- Environment variable support

### Enhanced Productivity
- No context switching between tools
- Natural command grouping by operation type
- Smart defaults and configuration
- Easier to document and teach to team members

## Backwards Compatibility

The underlying tools (`log_observation.py` and `memory-monitor`) remain fully functional:
- Existing scripts continue to work unchanged
- Advanced users can still access tools directly
- Debugging and troubleshooting specific tool issues
- Legacy automation and integration points

## Error Handling

```bash
# Unknown commands
flowloom-memory unknown-command
# Returns exit code 1 with helpful error message

# Missing arguments
flowloom-memory add-entity
# Routes to log_observation.py which provides specific help

# Invalid memory file
flowloom-memory --memory-file /nonexistent/file.json stats
# Returns appropriate error from underlying tool
```

## Performance Considerations

- Command routing adds minimal overhead (~1ms)
- All heavy operations delegated to underlying tools
- Memory file access patterns unchanged
- No additional memory usage for routing

## Future Enhancements

Planned improvements for the unified interface:
- Bash/Zsh completion support
- Configuration file support (.flowloom-memory.yaml)
- Enhanced pipeline integration features
- Cross-session command chaining
- Built-in templating for common operations

---

*The unified interface is the recommended way to interact with FlowLoom's memory system. For advanced use cases or debugging, the individual tools remain available.*