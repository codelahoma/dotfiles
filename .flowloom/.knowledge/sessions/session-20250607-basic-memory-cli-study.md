---
type: session
category: learning
tags: [basic-memory, cli, knowledge-management, documentation, workflow]
status: active
complexity: intermediate
priority: high
frequency: daily
audience: [developers]
introduced_in: "FlowLoom 2.0.0"
last_verified: "2025-06-07"
maintainer: FlowLoom Core Team
permalink: session-20250607-basic-memory-cli
---

# Session 2025-06-07: Basic Memory CLI Study

## Context
Rod requested I study the basic-memory CLI reference to understand standalone usage without MCP server. During this study, I discovered my previously developed FlowLoom knowledge schema and learned key workflow principles.

## Key Discoveries

### Basic Memory CLI Commands
- `basic-memory sync` - Syncs files with knowledge graph (--watch, --verbose)
- `basic-memory import` - Import from Claude conversations, projects, ChatGPT, memory-json
- `basic-memory tool write-note` - Direct note creation with stdin support
- `basic-memory project` - Multi-project management
- `basic-memory status` - System status (--verbose, --json)

### FlowLoom Knowledge Schema
Discovered comprehensive front matter schema I developed:
- Standard: type, tags, permalink
- FlowLoom extensions: category, status, depends_on, related_to, complexity, stability, priority, etc.

### Critical Workflow Learnings
Rod emphasized several key principles:
1. **Document everything as we go** - Build comprehensive knowledge base
2. **Coordinate systems** - Use both fl-memory.json and basic-memory CLI
3. **Use relative paths** - Avoid absolute paths for guardrail compliance
4. **Avoid environment variables** - Use command-line arguments instead
5. **Direct file creation** - Write to .knowledge structure, then sync

### Command Patterns Discovered
```bash
# FlowLoom memory (relative paths)
./.flowloom/bin/flowloom-memory --memory-file ./.flowloom/fl-memory.json <command>

# Basic memory (project argument)
uvx basic-memory --project ./.flowloom/.knowledge <command>

# Basic memory file workflow
# 1. Write file to .knowledge structure
# 2. Sync with: uvx basic-memory --project ./.flowloom/.knowledge sync
```

## Actions Taken
1. Studied CLI reference documentation from https://memory.basicmachines.co/docs/cli-reference
2. Found existing FlowLoom knowledge files with custom schema
3. Started documenting session in both fl-memory.json and basic-memory
4. Created todo list for comprehensive documentation workflow
5. Learned proper command patterns for both systems

## Next Steps
- Capture complete CLI reference in knowledge base
- Document FlowLoom schema with examples
- Establish systematic documentation workflow
- Ensure system synchronization
- Track methodology learnings

## Relations
- implements [[Basic Memory Integration]]
- relates_to [[FlowLoom Knowledge Schema]]
- depends_on [[Documentation Workflow]]
- documents [[Command Patterns]]

## Observations
- [workflow] Direct file creation followed by sync is cleaner than tool commands
- [pattern] Relative paths prevent guardrail issues with absolute paths
- [integration] Both memory systems complement each other well
- [schema] FlowLoom front matter provides rich metadata for enterprise use