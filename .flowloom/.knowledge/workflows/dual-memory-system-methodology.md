---
type: guide
category: workflows
tags: [methodology, dual-memory, configuration, knowledge-expansion]
status: active
complexity: intermediate
priority: high
frequency: daily
audience: [flowloom-ai]
introduced_in: "FlowLoom 2.0.0"
last_verified: "2025-06-07"
maintainer: FlowLoom Core Team
permalink: dual-memory-system-methodology
depends_on: [basic-memory, flowloom-memory]
related_to: [flowloom-documentation-methodology, knowledge-expansion-documentation-pattern]
---

# Dual Memory System Methodology

## Core Insight: Knowledge Expansion Must Be Documented

When FlowLoom's knowledge expands through learning and correction, that expansion must be captured in our knowledge systems, not left as ephemeral understanding.

## The Configuration vs Memory Distinction

### INCORRECT Pattern (Discovered 2025-06-07)
- Storing historical development narratives in CLAUDE.local.md
- Treating configuration files as memory repositories
- Mixing current settings with past event documentation

### CORRECT Pattern
- **CLAUDE.md**: Public project instructions and identity
- **CLAUDE.local.md**: Private configuration and triggers only
- **fl-memory.json**: Session-level development intelligence
- **basic-memory**: Persistent knowledge base with proper entity/relation structure

## Memory System Queries for Recent Activity

Instead of hardcoded historical narratives, use:
```bash
uvx basic-memory --project ./.flowloom/.knowledge tool recent-activity --timeframe 24h
cat fl-memory.json | jq '.observations | sort_by(.timestamp) | reverse'
```

## Knowledge Expansion Protocol

When FlowLoom learns something important:

1. **File-First Documentation**: Create markdown files in `.knowledge` structure
2. **Methodology Updates**: Update workflow documentation when patterns change
3. **Configuration Cleanup**: Remove obsolete information from config files
4. **Memory Sync**: Run `uvx basic-memory --project ./.flowloom/.knowledge sync`

## Key Learning: Status Summary Accuracy

The "stat" command should derive status from actual memory systems, not assume historical context exists in configuration files. This ensures:
- Accurate current state representation
- Proper memory system utilization
- No confusion between configuration and historical data

## Implementation Impact

This methodology ensures FlowLoom's expanding knowledge is:
- Properly captured and searchable
- Available across sessions
- Organized by appropriate system (config vs memory)
- Builds institutional knowledge rather than losing insights

## Observations
- [correction] Configuration files are not memory systems
- [pattern] File-first workflow prevents permission issues
- [methodology] Memory systems should be queried, not hardcoded
- [workflow] Knowledge expansion requires immediate documentation