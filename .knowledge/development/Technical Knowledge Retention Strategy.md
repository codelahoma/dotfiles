---
title: Technical Knowledge Retention Strategy
type: note
permalink: development/technical-knowledge-retention-strategy
---

# Technical Knowledge Retention Strategy

## Problem
Claude Code has specific technical quirks that need systematic retention:
- Bash tool commands run in subshells (environment changes don't persist)
- MCP filesystem server preferences (relative paths preferred)
- Tool selection patterns (prefer MCP tools per project config)

## Solution Implemented
Multi-layered approach for knowledge retention:

### 1. CLAUDE.md Documentation
Added "Tool Technical Quirks" section to project configuration:
- Bash tool limitations and subprocess behavior
- MCP tool patterns and preferences
- Always-available reference in project context

### 2. Memory Graph Tracking
Created systematic entities for technical knowledge:
- "Claude Code Technical Quirks" entity with observations
- "MCP Filesystem Server Patterns" entity
- Searchable via memory system commands

### 3. Automatic Memory Integration
Configured automatic tracking of technical insights with:
- Consistent shell PID detection
- Timestamped observations
- Cross-referencing between systems

## Benefits
- **Persistent context**: Technical knowledge survives session boundaries
- **Searchable patterns**: Memory system enables pattern lookup
- **Proactive application**: Configuration guides future tool selection
- **Systematic capture**: New quirks get documented as discovered

## Usage
- Reference CLAUDE.md for immediate quirk lookup
- Search memory graph for detailed technical patterns
- Update both systems when new quirks discovered