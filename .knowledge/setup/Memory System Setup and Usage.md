---
title: Memory System Setup and Usage
type: note
permalink: setup/memory-system-setup-and-usage
tags:
- '#memory'
- '#mcp'
- '#setup'
- '#knowledge-management'
---

# Memory System Setup and Usage

## FlowLoom's Dual Memory Architecture

FlowLoom uses two complementary memory systems that work together:

### 1. Structured Memory (`memory.json`)
- **Purpose**: Entity-relationship graph for code understanding
- **Location**: Project root `/memory.json`
- **Server**: `@modelcontextprotocol/server-memory`
- **Use For**: Code entities, relationships, task tracking

### 2. Narrative Memory (`.knowledge/`)
- **Purpose**: Human-readable documentation and notes
- **Location**: Project root `/.knowledge/`
- **Server**: `@codemuse/basic-memory`
- **Use For**: Documentation, decisions, context, learning

## Initial Setup

### File Structure Created by Installer
```
your-project/
├── .claude/
│   └── settings.local.json    # Permissions configured
├── .knowledge/                 # Created by basic-memory
│   ├── setup/                 # Setup documentation
│   ├── daily/                 # Daily notes
│   └── summaries.db          # SQLite index
├── .mcp.json                  # MCP server configuration
└── memory.json                # Entity graph
```

### MCP Configuration
The installer configures both memory servers in `.mcp.json`:
```json
{
  "mcpServers": {
    "memory": {
      "command": ["npx", "-y", "@modelcontextprotocol/server-memory"],
      "args": [],
      "enabled": true
    },
    "basic-memory": {
      "command": ["npx", "-y", "@codemuse/basic-memory"],
      "args": ["--directory", ".knowledge"],
      "enabled": true
    }
  }
}
```

## Using Structured Memory

### Creating Entities
Track important code elements:
```
/mcp memory create_entities
- Name: UserAuthSystem
- Type: component
- Observations: ["Handles user authentication", "Uses JWT tokens"]
```

### Creating Relations
Link entities together:
```
/mcp memory create_relations
- From: UserAuthSystem
- To: DatabaseConnection
- Type: depends_on
```

### Searching Memory
Find relevant information:
```
/mcp memory search_nodes "authentication"
```

## Using Narrative Memory

### Writing Notes
Create documentation:
```
/mcp basic-memory write_note
- Title: "API Design Decisions"
- Folder: "architecture"
- Content: Your documentation
```

### Searching Knowledge
Find existing documentation:
```
/mcp basic-memory search_notes "api design"
```

### Daily Notes
Automatic daily summaries:
```
/mcp basic-memory write_note
- Title: "2024-01-20 Daily Summary"
- Folder: "daily"
```

## Best Practices

### 1. Use Both Systems
- **Structure** for code relationships
- **Narrative** for human context
- They complement each other

### 2. Regular Updates
- Update entities when code changes
- Write daily summaries
- Document decisions as they happen

### 3. Consistent Naming
- Use clear entity names
- Organize notes in folders
- Tag related content

### 4. Session Integration
FlowLoom sessions automatically:
- Track work in memory
- Create session entities
- Link changes to sessions

## Common Patterns

### Feature Development
1. Create feature entity in structured memory
2. Write design note in narrative memory
3. Link implementation entities
4. Document decisions

### Bug Fixing
1. Create bug entity with observations
2. Link to affected components
3. Document investigation notes
4. Update when resolved

### Code Review
1. Search both memories for context
2. Add review observations to entities
3. Write summary notes

## Troubleshooting

### Memory Server Not Found
```bash
claude mcp list  # Check if servers are running
```

### Can't Write to Memory
- Check permissions in settings.local.json
- Verify .knowledge directory exists
- Ensure memory.json is writable

### Search Not Working
- Basic-memory builds index gradually
- Check summaries.db exists
- Try simpler search terms

## Advanced Features

### Memory Coordination
FlowLoom's coordination system can:
- Share memory between Claude instances
- Synchronize entity updates
- Merge knowledge bases

### Export and Backup
- `memory.json` is version-controlled
- `.knowledge/` can be backed up
- Both can be exported/imported

### Integration with Commands
Many FlowLoom commands use memory:
- `/flowloom:plan:mem` - Memory-aware planning
- `/flowloom:memory:track-progress` - Task tracking
- `/flowloom:docs:smart-daily` - Automated summaries

Remember: The memory system is your project's living documentation. Keep it updated and it will serve you well throughout development.