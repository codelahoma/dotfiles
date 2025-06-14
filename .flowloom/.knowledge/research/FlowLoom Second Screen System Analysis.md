---
title: FlowLoom Second Screen System Analysis
type: note
permalink: research/flow-loom-second-screen-system-analysis
tags:
- '#flowloom'
- '#second-screen'
- '#ui-system'
- '#markdown'
- '#research'
---

# FlowLoom Second Screen System Analysis

## Overview
The FlowLoom repository has implemented a sophisticated second screen functionality that creates auto-refreshing markdown displays in Chrome for enhanced development workflows.

## Core Components

### 1. Shell Script Launcher (`bin/second-screen`)
- Creates unique display files using shell PID
- Auto-detects git status and project state
- Opens display in Chrome/Chromium
- Provides initial template with current state table

### 2. Command System (`/.claude/commands/flowloom/ui/`)
**Key Commands:**
- `second-screen.md` - Creates and launches second screen
- `display-update.md` - Updates content dynamically
- `display-path.md` - Shows current display file path
- `display-append.md` - Adds content without replacing
- `display-replace.md` - Replaces specific content sections
- `display-history.md` - Manages display history
- `display-archive.md` - Archives display sessions

### 3. Two-Screen Concept Files
**Files in Repository Root:**
- `screen-presentation.md` - Primary content display (larger screen)
- `screen-conversation.md` - Real-time status and communication

## Technical Architecture

### File Management
- Display files stored in `displays/` directory
- Unique naming: `claude_display_${SHELL_PID}.md`
- History tracking in `displays/history/`
- Auto-cleanup and archival capabilities

### Content Structure
```markdown
# FlowLoom Second Screen Display
*Session PID: 12345 | 2025-06-13 14:30:15*

---

## Current State
| Property | Value |
|----------|-------|
| Working Directory | `/path/to/project` |
| Git Branch | `feature-branch` |
| Git Status | 3 uncommitted files |
| Auto-Track | ✅ ENABLED |

---

## Active Display
[Dynamic content here]

---
*Last updated: timestamp*
```

### Cross-Platform Support
- macOS: Uses `open -a "Google Chrome"`
- Linux: Supports `google-chrome` and `chromium-browser`
- Fallback: File path display if browser not found

## Use Cases Observed

### 1. Development Status Display
- Real-time project state
- Git branch and status information
- Auto-track system status
- Session identification

### 2. Content Presentation
- Code samples and documentation
- System architecture diagrams
- Progress tracking visualizations
- Task and milestone displays

### 3. Communication Stream
- Status updates with timestamps
- Milestone achievements
- System insights and breakthroughs
- Current focus areas

## Implementation Patterns

### 1. Session Management
- Shell PID for unique session identification
- Auto-detection of shell processes
- Session-specific display files
- Cross-session persistence

### 2. Auto-Refresh Integration
- Leverages Chrome's auto-refresh for markdown files
- Real-time content updates without manual refresh
- Seamless development workflow integration

### 3. Command Integration
- Slash commands for display management
- Dynamic content updates from Claude
- Scriptable display operations
- History and archival management

## Key Insights

### 1. Markdown as UI Layer
- Uses markdown files as dynamic display medium
- Browser auto-refresh creates real-time UI
- No complex web application needed
- File-based approach for simplicity

### 2. Multi-Screen Workflow
- Separates conversation from presentation
- Allows for focused content display
- Maintains conversation flow separately
- Enhanced development experience

### 3. Session Awareness
- Tracks individual shell sessions
- Maintains session-specific displays
- Enables parallel development streams
- Proper cleanup and management

## Integration Opportunities

### For Our Dotfiles Project
1. **GTD Display Integration**
   - Show current GTD context and next actions
   - Display project progress and priorities
   - Real-time agenda and task updates

2. **Plan Visualization**
   - Present current plan status and progress
   - Show plan relationships and dependencies
   - Display implementation roadmaps

3. **Knowledge Base Display**
   - Show related notes and documentation
   - Display search results and connections
   - Present research findings and insights

This second screen system represents a sophisticated approach to enhancing development workflows through markdown-based displays that integrate seamlessly with existing tools and processes.