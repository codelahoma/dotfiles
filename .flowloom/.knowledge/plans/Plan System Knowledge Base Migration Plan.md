---
title: Plan System Knowledge Base Migration Plan
type: note
permalink: plans/plan-system-knowledge-base-migration-plan
tags:
- '#migration'
- '#knowledge-base'
- '#plan-system'
- '#infrastructure'
---

# Plan System Knowledge Base Migration Plan

## Overview
Comprehensive plan to migrate all plan-related commands and scripts from file-based storage to knowledge base integration, improving persistence, searchability, and cross-session continuity.

## Phase 1: Foundation (Priority: High)

### 1.1 Create Knowledge Base Utilities
**Purpose**: Shared utilities for consistent knowledge base operations across all tools

**Components**:
- `kb_write_plan()` - Write plan with proper metadata
- `kb_find_plans()` - Search plans with filters (status, tags, date range)
- `kb_update_plan_status()` - Update plan lifecycle state
- `kb_get_plan()` - Retrieve specific plan by ID or title
- `kb_link_plans()` - Create relationships between plans

**Location**: `.flowloom/lib/kb_plan_utils.sh` (for shell scripts) and embedded in Claude commands

### 1.2 Design Plan Schema
**Purpose**: Unified structure for all plans in knowledge base

**Schema Elements**:
```yaml
title: Plan Title
plan_id: PLAN-2025-001 (auto-generated)
status: draft|active|in_progress|completed|archived
type: architecture|implementation|vision|detailed|subsection
created_date: 2025-06-13
updated_date: 2025-06-13
parent_plan: PLAN-2025-000 (if subsection)
tags: [gtd, infrastructure, feature]
priority: high|medium|low
metadata:
  estimated_duration: 2 weeks
  actual_duration: null
  completion_percentage: 0
content: |
  # Full plan content here
relations:
  implements: [note-id-1, note-id-2]
  blocks: [plan-id-3]
  requires: [plan-id-4]
```

## Phase 2: Shell Script Updates (Priority: High)

### 2.1 Update `new_plan`
- Replace file creation with `mcp__basic-memory__write_note`
- Generate plan IDs automatically
- Add interactive prompts for metadata
- Store in `plans/` folder in knowledge base

### 2.2 Update `find_plan`
- Replace `find` command with `mcp__basic-memory__search_notes`
- Add filters for status, type, tags
- Support searching by ID, title, or content
- Display results with metadata

### 2.3 Update `recent_plans`
- Use `mcp__basic-memory__recent_activity` with type filter
- Show plans with status indicators
- Include last updated timestamp
- Group by status (active, draft, etc.)

### 2.4 Update `review_plans`
- Query knowledge base for active/in_progress plans
- Show completion percentage and estimates
- Display plan hierarchy (parent/child relationships)
- Include quick status update capability

## Phase 3: Claude Command Updates (Priority: High)

### 3.1 Update Creation Commands
**Files**: `smart-create.md`, `detailed.md`, `highlevel.md`, `arch.md`, `vision.md`
- Add knowledge base write after plan generation
- Include proper metadata and tagging
- Create relationships to existing plans/notes
- Return plan ID for reference

### 3.2 Update Management Commands
**Files**: `update.md`, `review.md`, `status.md`, `mem.md`
- Search knowledge base first for existing plans
- Update plans in place with version history
- Track status changes with timestamps
- Show related plans and dependencies

### 3.3 Update `subsection.md`
- Maintain parent-child relationships
- Inherit metadata from parent plan
- Update parent plan progress when subsections complete
- Show plan hierarchy in output

## Phase 4: Workflow Mode Integration (Priority: High)

### 4.1 Plan Selection
- Replace file-based plan review with knowledge base search
- Show plans relevant to current context
- Filter by status and type
- Display plan metadata in selection menu

### 4.2 Progress Tracking
- Create workflow session notes
- Track completed steps in knowledge base
- Link workflow activities to plans
- Maintain workflow state across sessions

### 4.3 Session Management
- Create session start/end notes
- Track time spent on plans
- Record decisions and outcomes
- Enable workflow resume from last state

## Phase 5: Advanced Features (Priority: Medium)

### 5.1 Plan Lifecycle Management
- Automated status transitions
- Completion criteria checking
- Archival rules and processes
- Plan health indicators

### 5.2 Plan Analytics
- Time tracking and estimates
- Completion rate analysis
- Dependency impact analysis
- Plan velocity metrics

### 5.3 Plan Templates
- Reusable plan structures
- Template library in knowledge base
- Quick plan creation from templates
- Custom template creation

## Implementation Order

1. **Week 1**: Create utilities and schema (Phase 1)
2. **Week 1-2**: Update shell scripts (Phase 2)
3. **Week 2**: Update Claude commands (Phase 3)
4. **Week 2-3**: Integrate workflow mode (Phase 4)
5. **Week 3-4**: Add advanced features (Phase 5)

## Success Criteria

- All plans stored persistently in knowledge base
- Seamless cross-session plan access
- Improved plan discovery and search
- Better tracking of plan relationships
- Enhanced workflow continuity
- Reduced friction in plan management

## Migration Strategy

1. Keep existing file-based plans during transition
2. Create migration script to import existing plans
3. Run both systems in parallel initially
4. Validate knowledge base functionality
5. Deprecate file-based storage
6. Archive old plan files

This migration will transform the plan system into a robust, persistent, and intelligent planning infrastructure that leverages the full power of the knowledge base.