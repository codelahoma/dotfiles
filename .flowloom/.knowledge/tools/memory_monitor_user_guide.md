---
type: guide
category: tools
tags:
- memory
- cli
- sql
- query
- monitoring
- analytics
status: active
depends_on:
- fl-memory.json
- memory-monitor
related_to:
- log_observation_user_guide
- flowloom_memory_unified_interface
introduced_in: FlowLoom 2.0.0
last_verified: '2025-06-07'
complexity: advanced
stability: stable
priority: high
frequency: daily
audience:
- developers
- analysts
- power-users
commands:
- memory-monitor
- uvx memory-monitor
apis:
- websocket-server
- sql-interface
completeness: 90
needs_update: false
maintainer: FlowLoom Core Team
permalink: tools/memory-monitor-user-guide
---

# memory-monitor User Guide

FlowLoom's advanced query and monitoring tool for the memory graph database. Provides SQL-based querying, real-time monitoring, and analytics capabilities.

> **💡 Recommended**: Use the [unified interface](flowloom_memory_unified_interface.md) `flowloom-memory` for a better user experience. This guide covers the underlying tool for advanced use cases.

## Overview

`memory-monitor` is FlowLoom's sophisticated memory analysis tool that allows you to:
- Execute SQL queries against the memory graph
- Monitor memory changes in real-time
- Search entities with flexible patterns
- Analyze memory statistics and patterns
- Wait for specific conditions

## Installation & Access

```bash
# Should be available in PATH after FlowLoom installation
memory-monitor --help

# Enable verbose logging
memory-monitor -v <command>
```

## Commands

### query
Execute SQL queries against memory data with flexible output formats.

```bash
# Basic syntax
memory-monitor query <memory_file> "<SQL_QUERY>"

# Examples
memory-monitor query fl-memory.json "SELECT * FROM entities WHERE type = 'Feature'"
memory-monitor query fl-memory.json "SELECT name, type FROM entities ORDER BY created_at DESC LIMIT 10"

# Output formats
memory-monitor query -o json fl-memory.json "SELECT * FROM entities"
memory-monitor query -o table fl-memory.json "SELECT * FROM entities" 
memory-monitor query -o csv fl-memory.json "SELECT * FROM entities"

# Pagination
memory-monitor query --page 1 --page-size 10 fl-memory.json "SELECT * FROM entities"

# Count only
memory-monitor query --count-only fl-memory.json "SELECT * FROM entities WHERE type = 'Session'"
```

### search
Quick search for entities using flexible patterns.

```bash
# Basic syntax
memory-monitor search <memory_file> <pattern>

# Examples
memory-monitor search fl-memory.json "FlowLoom"
memory-monitor search fl-memory.json "authentication"
memory-monitor search fl-memory.json "Session-*"
```

### stats
Show comprehensive memory file statistics.

```bash
memory-monitor stats <memory_file>

# Example
memory-monitor stats fl-memory.json
```

### watch
Monitor memory file for changes in real-time.

```bash
# Basic syntax
memory-monitor watch <memory_file>

# Example - watch for changes and show updates
memory-monitor watch fl-memory.json
```

### wait-for
Wait for a specific query condition to be met (useful for automation).

```bash
# Basic syntax
memory-monitor wait-for <memory_file> "<SQL_CONDITION>"

# Examples
memory-monitor wait-for fl-memory.json "SELECT COUNT(*) FROM entities WHERE type = 'Task' AND status = 'completed' >= 5"
memory-monitor wait-for fl-memory.json "SELECT * FROM entities WHERE name = 'Critical Feature'"
```

### serve
Start WebSocket server for real-time memory updates.

```bash
# Basic syntax
memory-monitor serve <memory_file>

# Example
memory-monitor serve fl-memory.json
# Server will start and provide WebSocket endpoint for real-time updates
```

### explain
Show query execution plan for optimization.

```bash
# Basic syntax
memory-monitor explain <memory_file> "<SQL_QUERY>"

# Example
memory-monitor explain fl-memory.json "SELECT * FROM entities WHERE type = 'Feature' AND created_at > '2025-01-01'"
```

## SQL Schema

The memory database provides these main tables:

### entities
- `name` (TEXT): Unique entity identifier
- `type` (TEXT): Entity type (Feature, Component, etc.)
- `created_at` (TIMESTAMP): Creation time
- `updated_at` (TIMESTAMP): Last modification time

### observations
- `id` (INTEGER): Unique observation ID
- `entity_name` (TEXT): Associated entity name
- `content` (TEXT): Observation content
- `timestamp` (TIMESTAMP): When observation was made

### relations
- `id` (INTEGER): Unique relation ID
- `from_entity` (TEXT): Source entity name
- `to_entity` (TEXT): Target entity name
- `relation_type` (TEXT): Type of relationship
- `created_at` (TIMESTAMP): When relationship was created

## Common Query Patterns

### Entity Analysis
```sql
-- Recent entities
SELECT name, type, created_at FROM entities ORDER BY created_at DESC LIMIT 10;

-- Entities by type
SELECT type, COUNT(*) as count FROM entities GROUP BY type;

-- Entity activity
SELECT e.name, COUNT(o.id) as observation_count 
FROM entities e LEFT JOIN observations o ON e.name = o.entity_name 
GROUP BY e.name ORDER BY observation_count DESC;
```

### Relationship Analysis
```sql
-- Most connected entities
SELECT from_entity, COUNT(*) as outgoing_relations 
FROM relations GROUP BY from_entity ORDER BY outgoing_relations DESC;

-- Relationship types
SELECT relation_type, COUNT(*) as count FROM relations GROUP BY relation_type;

-- Find dependencies
SELECT from_entity, to_entity FROM relations WHERE relation_type = 'depends_on';
```

### Temporal Analysis
```sql
-- Recent observations
SELECT entity_name, content, timestamp FROM observations 
ORDER BY timestamp DESC LIMIT 20;

-- Activity by day
SELECT DATE(timestamp) as day, COUNT(*) as observations 
FROM observations GROUP BY DATE(timestamp) ORDER BY day DESC;
```

### Search and Filter
```sql
-- Find entities containing text
SELECT * FROM entities WHERE name LIKE '%authentication%';

-- Complex filtering
SELECT e.name, e.type, COUNT(o.id) as obs_count
FROM entities e LEFT JOIN observations o ON e.name = o.entity_name
WHERE e.type IN ('Feature', 'Component') 
GROUP BY e.name, e.type
HAVING obs_count > 2;
```

## Real-time Monitoring

Use the `watch` command to monitor changes:

```bash
# Monitor all changes
memory-monitor watch fl-memory.json

# Monitor with verbose output
memory-monitor -v watch fl-memory.json
```

Use `wait-for` in scripts:

```bash
# Wait for completion
memory-monitor wait-for fl-memory.json "SELECT COUNT(*) FROM entities WHERE type = 'Task' AND status = 'done' >= 10"
echo "All tasks completed!"
```

## Integration with FlowLoom Workflow

1. **Development Monitoring**: Use `watch` during active development
2. **Progress Tracking**: Query task and session entities for status
3. **Dependency Analysis**: Use relationship queries for architecture review
4. **Reporting**: Generate reports with formatted output options
5. **Automation**: Use `wait-for` in CI/CD pipelines
6. **Real-time Updates**: Use `serve` for dashboard integrations

## Performance Tips

- Use `LIMIT` for large datasets
- Index on frequently queried columns
- Use `--count-only` for quick counts
- Use `explain` to optimize complex queries
- Paginate large result sets

## Output Formats

- **table**: Human-readable tabular format (default)
- **json**: Machine-readable JSON format
- **csv**: Comma-separated values for spreadsheets

Choose format based on your use case:
- Use `table` for manual review
- Use `json` for programmatic processing
- Use `csv` for data analysis tools