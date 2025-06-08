---
type: session
category: startup
tags: [current-context, active-state, session-summary, methodology]
status: active
complexity: intermediate
priority: high
frequency: daily
audience: [flowloom-ai]
introduced_in: "FlowLoom 2.0.0"
last_verified: "2025-06-07"
maintainer: FlowLoom Core Team
permalink: current-working-context
startup_essential: true
context_category: current_state
---

# Current Working Context - 2025-06-07

## Active Session Summary
Currently working on establishing comprehensive knowledge management methodology for FlowLoom AI. Key focus areas:

### Primary Objectives
1. **Knowledge Capture System** - Dual system with fl-memory.json + basic-memory
2. **Methodology Documentation** - Systematic workflow to prevent repetition
3. **Command Development** - Claude commands for knowledge management
4. **Context Recovery** - Tools for FlowLoom AI to regain context quickly

### Recent Discoveries (Last Session)
- **Basic Memory CLI Study** - Learned standalone usage patterns
- **FlowLoom Knowledge Schema** - Discovered comprehensive front matter system
- **Command Patterns** - Critical syntax for both memory systems
- **Configuration Mode** - Claude Code `/project:mode:config` capability

### Critical Methodological Rules
Rod emphasized these must be remembered:

1. **Document Everything As You Go**
   - Build comprehensive knowledge base
   - Prevent repetitive explanations
   - Use both fl-memory.json and basic-memory systems

2. **Command Syntax Requirements**
   - Use relative paths from project root (never absolute)
   - Use command arguments instead of environment variables
   - Use --from-file with .flowloom/tmp for rich content
   - Follow specific patterns to avoid guardrail issues

3. **Dual System Coordination**
   - fl-memory.json: Entity tracking, relationships, development progress
   - basic-memory: Structured documentation, guides, references
   - Keep both synchronized and cross-referenced

### Active Memory Systems

#### FlowLoom Memory Commands
```bash
# Entity operations
./.flowloom/bin/flowloom-memory --memory-file ./.flowloom/fl-memory.json add-entity "Name" "Type" "Description"
./.flowloom/bin/flowloom-memory --memory-file ./.flowloom/fl-memory.json add-observation "Name" "Type" --from-file ./.flowloom/tmp/file.md
./.flowloom/bin/flowloom-memory --memory-file ./.flowloom/fl-memory.json add-relation "From" "To" "relation_type"

# Queries
./.flowloom/bin/flowloom-memory --memory-file ./.flowloom/fl-memory.json search "term"
./.flowloom/bin/flowloom-memory --memory-file ./.flowloom/fl-memory.json stats
```

#### Basic Memory Commands
```bash
# File-based workflow (PREFERRED)
# 1. Write markdown to .knowledge structure with proper front matter
# 2. Sync: uvx basic-memory --project ./.flowloom/.knowledge sync

# Direct operations
uvx basic-memory --project ./.flowloom/.knowledge tool write-note --title "Title" --folder "folder"
uvx basic-memory --project ./.flowloom/.knowledge tool search-notes --query "term"
uvx basic-memory --project ./.flowloom/.knowledge status
```

### Current Project Structure
```
.flowloom/
├── fl-memory.json          # Entity graph database
├── .knowledge/             # Basic memory knowledge base
│   ├── startup/           # Startup context and recovery
│   ├── sessions/          # Development sessions
│   ├── workflows/         # Process documentation
│   ├── tools/             # Tool guides and references
│   └── .basic-memory/     # Database files
├── bin/                   # FlowLoom tools
│   ├── flowloom-memory    # Unified memory interface
│   └── ...
└── tmp/                   # Temporary files for rich content
```

### Active Entities (fl-memory.json)
- **Session-20250607-2042** - Current learning session
- **Documentation Methodology** - Core workflow system
- **Configuration Mode** - Newly discovered Claude capability

### Recent Knowledge Files (basic-memory)
- `workflows/flowloom-documentation-methodology.md` - Complete methodology
- `sessions/session-20250607-basic-memory-cli-study.md` - Session documentation
- `tools/flowloom_memory_unified_interface.md` - Tool documentation

### Next Steps in Progress
- Capture complete basic-memory CLI reference
- Document FlowLoom front matter schema with examples
- Create startup command for context recovery
- Ensure system synchronization

## Key Learning Points
- **Context Loss is Inevitable** - Documentation prevents repetition
- **Dual Systems Provide Redundancy** - Different access patterns
- **Relative Paths Prevent Issues** - Guardrails and permissions
- **Rich Content Needs Special Handling** - Use --from-file patterns

## Relations
- continues [[Session-20250607-2042]]
- implements [[Documentation Methodology]]
- prepares_for [[Startup Command Development]]

## Observations
- [context] This represents current state for quick recovery
- [methodology] Captures all essential working knowledge
- [workflow] Enables immediate productivity after context loss