---
type: guide
category: workflows
tags: [documentation, methodology, basic-memory, flowloom-memory, knowledge-capture]
status: active
complexity: intermediate
priority: high
frequency: daily
audience: [developers, flowloom-ai]
introduced_in: "FlowLoom 2.0.0"
last_verified: "2025-06-07"
maintainer: FlowLoom Core Team
permalink: flowloom-documentation-methodology
depends_on: [basic-memory, flowloom-memory, fl-memory.json]
related_to: [session-20250607-basic-memory-cli, flowloom_memory_unified_interface]
---

# FlowLoom Documentation Methodology

## Core Principle
**Document everything as you go** - Build comprehensive knowledge base to prevent repetition and context loss.

## Dual System Architecture

### 1. FlowLoom Memory (fl-memory.json)
- **Purpose**: Session tracking, entity relationships, development progress
- **Format**: JSON-based graph database
- **Command Pattern**: 
  ```bash
  ./.flowloom/bin/flowloom-memory --memory-file ./.flowloom/fl-memory.json <command>
  ```

### 2. Basic Memory (.knowledge)
- **Purpose**: Structured knowledge documentation, guides, references
- **Format**: Markdown files with front matter
- **Workflow**:
  1. Write markdown files directly to `.knowledge` structure
  2. Include proper front matter with FlowLoom schema
  3. Sync with: `uvx basic-memory --project ./.flowloom/.knowledge sync`

## Command Patterns (CRITICAL TO REMEMBER)

### FlowLoom Memory Commands
```bash
# Entity management
./.flowloom/bin/flowloom-memory --memory-file ./.flowloom/fl-memory.json add-entity "Name" "Type" "Description"
./.flowloom/bin/flowloom-memory --memory-file ./.flowloom/fl-memory.json add-observation "Name" "Type" "Observation"
./.flowloom/bin/flowloom-memory --memory-file ./.flowloom/fl-memory.json add-relation "From" "To" "relation_type"

# Rich content observations (PREFERRED for complex content)
cat > ./.flowloom/tmp/temp-observation.md << 'EOF'
Rich observation content...
- Markdown formatting
- Code blocks  
- Multiple paragraphs
EOF
./.flowloom/bin/flowloom-memory --memory-file ./.flowloom/fl-memory.json add-observation "Name" "Type" --from-file ./.flowloom/tmp/temp-observation.md

# Queries
./.flowloom/bin/flowloom-memory --memory-file ./.flowloom/fl-memory.json search "term"
./.flowloom/bin/flowloom-memory --memory-file ./.flowloom/fl-memory.json stats
```

### Basic Memory Commands
```bash
# Project-based operations (NO environment variables)
uvx basic-memory --project ./.flowloom/.knowledge sync --verbose
uvx basic-memory --project ./.flowloom/.knowledge status
uvx basic-memory --project ./.flowloom/.knowledge tool write-note --title "Title" --folder "folder"

# File-based workflow (PREFERRED)
# 1. Create file: Write markdown to .knowledge structure
# 2. Sync: uvx basic-memory --project ./.flowloom/.knowledge sync
```

## Path Requirements (CRITICAL)
- **Always use relative paths** from project root
- **Never use absolute paths** - causes guardrail issues
- **Never use environment variables** - requires permission prompts
- **Use command-line arguments** instead

## FlowLoom Front Matter Schema

### Required Fields
```yaml
type: session|guide|reference|documentation|workflow
category: tools|workflows|learning|architecture
tags: [tag1, tag2, tag3]
status: active|deprecated|experimental
```

### Extended Fields
```yaml
complexity: beginner|intermediate|advanced
priority: high|medium|low
frequency: daily|weekly|monthly|rare
audience: [developers, end-users, flowloom-ai]
introduced_in: "FlowLoom 2.0.0"
last_verified: "2025-06-07"
maintainer: FlowLoom Core Team
permalink: custom-permalink
depends_on: [dependency1, dependency2]
related_to: [related1, related2]
file_paths: [".flowloom/bin/script.py"]
commands: [command1, command2]
completeness: 0-100
needs_update: true|false
review_date: "YYYY-MM-DD"
```

## Knowledge Structure
```
.flowloom/.knowledge/
├── sessions/           # Development sessions, learning sessions
├── workflows/          # Process documentation, methodologies
├── tools/              # Tool guides, CLI references
├── architecture/       # System design, schemas
├── reference/          # Quick reference materials
└── .basic-memory/      # Basic memory database files
```

## Documentation Workflow

### For Every Session
1. **Create session file** in `sessions/` with context and discoveries
2. **Log key entities** in fl-memory.json for relationships
3. **Document methodology** if new patterns discovered
4. **Update related documents** if dependencies change

### For New Knowledge
1. **Determine category** (sessions, workflows, tools, etc.)
2. **Create structured file** with full front matter
3. **Include observations** with [category] tags
4. **Map relations** to existing knowledge
5. **Sync to database** with basic-memory

### For Lost Context Recovery
1. **Search both systems**: basic-memory search + flowloom-memory search
2. **Check recent sessions**: Look in sessions/ folder
3. **Review entity relationships**: Query fl-memory.json graph
4. **Check methodology docs**: This file and related workflows

## Anti-Patterns to Avoid
- ❌ Using absolute paths instead of relative
- ❌ Using environment variables instead of arguments  
- ❌ Skipping documentation "to save time"
- ❌ Single-system documentation (use both)
- ❌ Missing front matter schema
- ❌ Forgetting to sync after file creation

## Success Patterns
- ✅ Document immediately during discovery
- ✅ Use both fl-memory.json and basic-memory
- ✅ Relative paths with command arguments
- ✅ Rich front matter for discoverability
- ✅ Cross-reference between systems
- ✅ Regular sync operations
- ✅ Use --from-file with .flowloom/tmp for rich observations
- ✅ Temporary files in .flowloom/tmp avoid guardrails

## Emergency Context Recovery
When FlowLoom AI loses context:

1. **Read this methodology file first**
2. **Check recent sessions**: `ls ./.flowloom/.knowledge/sessions/`
3. **Search for relevant topics**: 
   ```bash
   uvx basic-memory --project ./.flowloom/.knowledge tool search-notes --query "topic"
   ./.flowloom/bin/flowloom-memory --memory-file ./.flowloom/fl-memory.json search "topic"
   ```
4. **Review current todos**: Use TodoRead tool
5. **Check active entities**: 
   ```bash
   ./.flowloom/bin/flowloom-memory --memory-file ./.flowloom/fl-memory.json list-entities --type Session
   ```

## Observations
- [critical] Context loss is inevitable - documentation prevents repetition
- [pattern] Dual system provides redundancy and different access patterns
- [workflow] File-first approach with sync is cleaner than tool commands
- [integration] Relative paths prevent permission and guardrail issues
- [schema] Rich front matter enables sophisticated knowledge graph queries

## Relations
- implements [[Knowledge Management Strategy]]
- depends_on [[Basic Memory CLI]], [[FlowLoom Memory System]]
- relates_to [[Session Documentation]], [[Workflow Documentation]]
- enables [[Context Recovery]], [[Knowledge Discovery]]