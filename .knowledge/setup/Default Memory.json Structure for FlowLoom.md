---
title: Default Memory.json Structure for FlowLoom
type: note
permalink: setup/default-memory-json-structure-for-flow-loom
tags:
- '#memory'
- '#installation'
- '#defaults'
- '#entities'
---

# Default Memory.json Structure for FlowLoom

## Core System Entities

The bundled memory.json should contain these essential entities to help users understand FlowLoom's architecture:

### 1. FlowLoom Core Components

```json
{
  "entities": {
    "flowloom-core": {
      "name": "FlowLoom Core",
      "type": "system",
      "observations": [
        "Central coordination system for multi-Claude workflows",
        "Manages session lifecycle and memory synchronization",
        "Provides slash commands via .claude/commands structure"
      ]
    },
    "session-manager": {
      "name": "Session Manager",
      "type": "component",
      "observations": [
        "Tracks work sessions with unique IDs",
        "Supports both simple and worktree-isolated sessions",
        "Maintains session state in .flowloom/sessions/"
      ]
    },
    "memory-system": {
      "name": "Dual Memory System",
      "type": "component",
      "observations": [
        "Structured memory in memory.json for entities/relations",
        "Narrative memory in .knowledge/ for documentation",
        "Both systems work together for complete project understanding"
      ]
    },
    "command-system": {
      "name": "Command System",
      "type": "component", 
      "observations": [
        "Slash commands stored in .claude/commands/",
        "Supports argument processing and command chaining",
        "Extensible through markdown command definitions"
      ]
    }
  }
}
```

### 2. Key Workflow Entities

```json
{
  "git-workflow": {
    "name": "Git Integration",
    "type": "workflow",
    "observations": [
      "Uses bash git commands, NOT mcp__git server",
      "Sync command provides streamlined git operations",
      "Supports worktree isolation for parallel development"
    ]
  },
  "coordination-workflow": {
    "name": "Multi-Claude Coordination",
    "type": "workflow",
    "observations": [
      "Conductor mode manages work distribution",
      "Worker mode executes assigned tasks",
      "Memory synchronization through shared entities"
    ]
  },
  "development-workflow": {
    "name": "Development Workflow",
    "type": "workflow",
    "observations": [
      "Plan-based development with numbered plans",
      "Session tracking for work continuity",
      "Integrated testing and documentation"
    ]
  }
}
```

### 3. Configuration Entities

```json
{
  "claude-local-config": {
    "name": "CLAUDE.local.md",
    "type": "configuration",
    "observations": [
      "Project-specific Claude configuration",
      "Defines slashload command behavior",
      "Not committed to version control"
    ]
  },
  "mcp-config": {
    "name": "MCP Configuration",
    "type": "configuration",
    "observations": [
      "Configures MCP servers in .mcp.json",
      "Filesystem server requires allowedDirectories",
      "Memory and basic-memory servers for knowledge management"
    ]
  },
  "settings-config": {
    "name": "Settings Configuration",
    "type": "configuration",
    "observations": [
      "Permissions defined in .claude/settings.local.json",
      "Pre-approved permissions for FlowLoom operations",
      "Bash command permissions with specific syntax"
    ]
  }
}
```

### 4. Important Relations

```json
{
  "relations": [
    {
      "from": "session-manager",
      "to": "flowloom-core",
      "type": "component_of"
    },
    {
      "from": "memory-system",
      "to": "flowloom-core",
      "type": "component_of"
    },
    {
      "from": "command-system",
      "to": "flowloom-core",
      "type": "component_of"
    },
    {
      "from": "git-workflow",
      "to": "session-manager",
      "type": "uses"
    },
    {
      "from": "coordination-workflow",
      "to": "memory-system",
      "type": "depends_on"
    },
    {
      "from": "development-workflow",
      "to": "command-system",
      "type": "uses"
    },
    {
      "from": "mcp-config",
      "to": "memory-system",
      "type": "configures"
    },
    {
      "from": "settings-config",
      "to": "command-system",
      "type": "enables"
    }
  ]
}
```

### 5. Getting Started Entities

```json
{
  "getting-started": {
    "name": "Getting Started Guide",
    "type": "documentation",
    "observations": [
      "Run /flowloom:system:welcome for introduction",
      "Start sessions with /flowloom:session:start",
      "Use 'sync' for git operations"
    ]
  },
  "common-commands": {
    "name": "Common FlowLoom Commands",
    "type": "reference",
    "observations": [
      "/flowloom:commands - List all available commands",
      "/flowloom:session:status - Check current session",
      "/flowloom:memory:query - Search memory system",
      "/flowloom:plan:create - Create development plans"
    ]
  }
}
```

## Benefits of Pre-populated Memory

1. **Immediate Context**: New users understand system architecture
2. **Discoverable Features**: Entities reveal available functionality
3. **Relationship Understanding**: Shows how components interact
4. **Learning Path**: Observations guide exploration
5. **Best Practices**: Embedded knowledge prevents common mistakes

## Usage Pattern

When users install FlowLoom:
1. They get a pre-populated knowledge graph
2. Can immediately query: "What is session-manager?"
3. Discover relationships: "What uses memory-system?"
4. Learn workflows: "Show coordination-workflow observations"
5. Extend with project-specific entities

This approach makes FlowLoom self-documenting from installation!