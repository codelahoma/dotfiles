# FlowLoom Startup Context Stdlib

## Context Gathering Logic

This stdlib prompt handles comprehensive startup context gathering for FlowLoom AI sessions.

**Arguments**: 
- Empty: Full startup context (default)
- "quick": Essential context only
- "session": Recent session focus  
- "commands": Command reference summary

## Implementation

Read essential startup documents:

```bash
# Startup context index
mcp__basic-memory__read_note startup-context-index

# Current working context  
mcp__basic-memory__read_note current-working-context

# Recent sessions
mcp__basic-memory__search_notes session --types session

# FlowLoom memory stats
./.flowloom/bin/flowloom-memory --memory-file ./.flowloom/fl-memory.json stats

# Current todos
TodoRead
```

Present structured summary:

### 🎯 **Current Objectives**
- Active goals from todos and context

### 📋 **Essential Command Patterns**  
- FlowLoom memory: `./.flowloom/bin/flowloom-memory --memory-file ./.flowloom/fl-memory.json <cmd>`
- Basic memory MCP: `mcp__basic-memory__*` tools
- Basic memory CLI: `uvx basic-memory --project ./.flowloom/.knowledge <cmd>`

### 🔄 **Recent Progress**
- Latest session findings
- New discoveries and learnings
- Methodological updates

### ⚡ **Quick Actions Available**
- Common next steps based on current state
- Ready-to-use command examples  
- Priority items from todo list

### 🚨 **Critical Reminders**
- Document everything as you go - prevent repetition
- Use both memory systems - fl-memory.json + basic-memory
- Relative paths only - avoid absolute paths and env vars
- Rich content via temp files - use .flowloom/tmp and --from-file
- Systematic organization - proper front matter and tagging

**FlowLoom AI ready for productive work!**