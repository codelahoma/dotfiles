---
type: guide
category: tools
tags: [memory, cli, entities, observations, direct-access]
status: active
depends_on: [fl-memory.json]
related_to: [memory_monitor_user_guide, flowloom_memory_unified_interface]
introduced_in: "FlowLoom 2.0.0"
last_verified: "2025-06-07"
complexity: intermediate
stability: stable
priority: medium
frequency: daily
audience: [developers, power-users]
file_paths: [".flowloom/bin/log_observation.py"]
commands: [log_observation.py, python log_observation.py]
completeness: 95
needs_update: false
maintainer: FlowLoom Core Team
---

# log_observation.py User Guide

FlowLoom's direct memory manager for managing entities, observations, and relationships in the fl-memory.json graph database.

> **💡 Recommended**: Use the [unified interface](flowloom_memory_unified_interface.md) `flowloom-memory` for a better user experience. This guide covers the underlying tool for advanced use cases.

## Overview

`log_observation.py` provides command-line access to FlowLoom's memory system, allowing you to:
- Create and manage entities with typed observations
- Build relationships between entities  
- Query and explore the knowledge graph
- Track development progress and decisions

## Basic Usage

```bash
# Location
/Users/rodk/.homesick/repos/dotfiles/.flowloom/bin/log_observation.py

# Help
python log_observation.py --help
python log_observation.py <command> --help
```

## Commands

### add-entity
Create new entity or update existing with observation.

```bash
# Basic syntax
python log_observation.py add-entity "Entity Name" "Type" "Observation text"

# Examples
python log_observation.py add-entity "FlowLoom Memory System" "Feature" "Implemented JSONL format for clean git merges"
python log_observation.py add-entity "Session-12345" "Session" "Completed authentication implementation"

# From file (for complex/long text)
python log_observation.py add-entity "Architecture Decision" "Documentation" --from-file decision.md
```

### add-observation
Add observation to existing entity.

```bash
# Basic syntax
python log_observation.py add-observation "Entity Name" "Additional observation"

# Examples  
python log_observation.py add-observation "FlowLoom Memory System" "Added support for relationship queries"
python log_observation.py add-observation "Session-12345" "Fixed authentication edge case"
```

### add-relation
Create relationship between two entities.

```bash
# Basic syntax
python log_observation.py add-relation "Source Entity" "Target Entity" "relation_type"

# Examples
python log_observation.py add-relation "Authentication Module" "Database Layer" "depends_on"
python log_observation.py add-relation "FlowLoom UI" "WebSocket Bridge" "uses"
python log_observation.py add-relation "Memory System" "Basic Memory" "implements"
```

### get-entity
Retrieve detailed information about a specific entity.

```bash
# Basic syntax
python log_observation.py get-entity "Entity Name"

# Examples
python log_observation.py get-entity "FlowLoom Memory System"
python log_observation.py get-entity "Session-12345"
```

### list-entities
List all entities or filter by type.

```bash
# List all entities
python log_observation.py list-entities

# Filter by type
python log_observation.py list-entities --type Feature
python log_observation.py list-entities --type Session
python log_observation.py list-entities --type Component
```

### stats
Show memory statistics and summary.

```bash
python log_observation.py stats
```

## Entity Types

Common entity types used in FlowLoom:

- **Feature**: Major functionality or capabilities
- **Component**: Software components, modules, classes
- **Session**: Development sessions, user interactions
- **Task**: Specific work items or todos
- **Documentation**: Guides, decisions, specifications
- **System**: Infrastructure, tools, environments
- **User**: People, personas, stakeholders

## Relation Types

Common relationship types:

- **depends_on**: Dependency relationships
- **implements**: Implementation relationships
- **contains**: Containment/composition
- **relates_to**: General associations
- **creates**: Creation relationships
- **uses**: Usage relationships

## Configuration

The tool uses `fl-memory.json` in the current directory by default. Override with:

```bash
python log_observation.py --memory-file /path/to/custom-memory.json <command>
```

## Integration Workflow

1. **During Development**: Log entities and observations as you work
2. **Session Tracking**: Create session entities with progress observations
3. **Relationship Mapping**: Connect related components and features
4. **Decision Recording**: Capture architectural decisions and rationale
5. **Query and Review**: Use get-entity and list-entities to review progress

## Best Practices

- Use descriptive, unique entity names
- Be consistent with entity types across the project
- Add observations incrementally as work progresses
- Establish relationships to build a connected knowledge graph
- Use --from-file for complex observations like code snippets or long decisions