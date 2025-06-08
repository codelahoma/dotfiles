---
type: reference
category: startup
tags: [startup, context, methodology, workflow, quick-reference, essential]
status: active
complexity: beginner
priority: high
frequency: daily
audience: [flowloom-ai]
introduced_in: "FlowLoom 2.0.0"
last_verified: "2025-06-07"
maintainer: FlowLoom Core Team
permalink: startup-context-index
startup_essential: true
context_category: startup_required
---

# FlowLoom Startup Context Index

## Essential Startup Knowledge

This document serves as the primary index for all startup-critical knowledge that FlowLoom AI needs to recall immediately upon initialization.

### Core Methodology Documents
- [[Documentation Methodology]] - Complete workflow for dual knowledge systems
- [[FlowLoom Knowledge Schema]] - Front matter schema and standards
- [[Command Patterns]] - Essential command syntax and guardrail compliance

### Active Session Context
- Latest session findings and discoveries
- Current project state and active entities
- Recent methodological learnings

### Quick Reference Commands

#### FlowLoom Memory (fl-memory.json)
```bash
./.flowloom/bin/flowloom-memory --memory-file ./.flowloom/fl-memory.json <command>
```

#### Basic Memory (.knowledge)
```bash
uvx basic-memory --project ./.flowloom/.knowledge <command>
```

#### Key Patterns
- Use relative paths from project root
- Use command arguments, not environment variables
- Use --from-file with .flowloom/tmp for rich content
- Document everything in both systems

### Critical Rules to Remember
1. **Document everything as you go** - prevent repetition
2. **Use both memory systems** - fl-memory.json + basic-memory
3. **Relative paths only** - avoid absolute paths and env vars
4. **Rich content via temp files** - use .flowloom/tmp and --from-file
5. **Systematic organization** - proper front matter and tagging

### Emergency Context Recovery
When context is lost:
1. Read this index first
2. Check recent sessions in `sessions/`
3. Search both memory systems for relevant topics
4. Review current todos with TodoRead
5. Query active entities in fl-memory.json

## Relations
- indexes [[Documentation Methodology]]
- references [[Session Documentation]]
- enables [[Context Recovery]]
- guides [[Startup Procedures]]

## Observations
- [startup] This index prevents context loss and repetitive explanations
- [workflow] Serves as entry point for all FlowLoom knowledge
- [methodology] Contains the essential rules for systematic documentation