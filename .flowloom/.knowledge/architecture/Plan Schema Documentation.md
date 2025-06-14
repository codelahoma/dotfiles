---
title: Plan Schema Documentation
type: note
permalink: architecture/plan-schema-documentation
tags:
- '#schema'
- '#plans'
- '#knowledge-base'
- '#documentation'
---

# Plan Schema Documentation

## Overview
This document defines the unified schema for storing plans in the knowledge base. All plans follow this structure to ensure consistency and enable advanced features like lifecycle management, relationships, and analytics.

## Schema Definition

### Core Metadata (YAML Frontmatter)
```yaml
---
plan_id: PLAN-2025-1718256000         # Unique identifier
title: Plan Title                     # Human-readable title
status: draft|active|in_progress|completed|archived
type: architecture|implementation|vision|detailed|subsection
created_date: 2025-06-13             # ISO date format
updated_date: 2025-06-13             # Last modification
author: Claude/Human                  # Plan creator
priority: high|medium|low            
estimated_duration: 2 weeks          # Human-readable duration
actual_duration: null                # Filled on completion
completion_percentage: 0             # 0-100
parent_plan: PLAN-2025-1718255000   # For hierarchical plans
tags: [gtd, feature, infrastructure] # Searchable tags
---
```

### Content Structure
```markdown
# Plan Title

## Executive Summary
Brief overview of what this plan accomplishes.

## Objectives
- Primary objective 1
- Primary objective 2
- Primary objective 3

## Context
Background information and why this plan exists.

## Scope
### In Scope
- What this plan covers

### Out of Scope
- What this plan explicitly doesn't cover

## Approach
Detailed approach to achieving objectives.

## Timeline
- Week 1: Foundation work
- Week 2: Core implementation
- Week 3: Testing and refinement
- Week 4: Documentation and deployment

## Success Criteria
- Measurable outcome 1
- Measurable outcome 2
- Measurable outcome 3

## Dependencies
- Dependency 1
- Dependency 2

## Risks and Mitigations
- Risk 1: Description → Mitigation strategy
- Risk 2: Description → Mitigation strategy
```

### Relationship Tracking
Plans can have relationships defined in a relations section:
```yaml
relations:
  implements: [note-id-1, note-id-2]  # What this plan implements
  blocks: [PLAN-2025-001]             # Plans blocked by this
  requires: [PLAN-2025-002]           # Prerequisites
  supersedes: [PLAN-2025-003]         # Replaces these plans
  children: [PLAN-2025-004, PLAN-2025-005]  # Sub-plans
```

## Plan Types

### Architecture Plans
- High-level system design
- Technology decisions
- Integration patterns
- Long-term vision

### Implementation Plans
- Concrete steps to build features
- Technical specifications
- Testing strategies
- Deployment procedures

### Vision Plans
- Product direction
- Strategic initiatives
- Long-term goals
- Success metrics

### Detailed Plans
- Step-by-step procedures
- Specific task breakdowns
- Resource allocations
- Precise timelines

### Subsection Plans
- Components of larger plans
- Focused scope
- Inherit context from parent
- Update parent progress

## Status Lifecycle

```
draft → active → in_progress → completed → archived
  ↓        ↓          ↓             ↓
  → cancelled      → blocked    → superseded
```

### Status Definitions
- **draft**: Initial planning stage
- **active**: Approved and ready to execute
- **in_progress**: Currently being implemented
- **completed**: Successfully finished
- **archived**: No longer relevant but kept for history
- **cancelled**: Abandoned before completion
- **blocked**: Waiting on dependencies
- **superseded**: Replaced by newer plan

## Metadata Evolution

Plans accumulate metadata over their lifecycle:
```yaml
# At creation
created_date: 2025-06-13
status: draft

# When activated
activated_date: 2025-06-14
status: active

# During execution
started_date: 2025-06-15
status: in_progress
completion_percentage: 45
last_progress_update: 2025-06-20

# At completion
completed_date: 2025-06-27
status: completed
actual_duration: 2 weeks
completion_percentage: 100
lessons_learned: |
  - Lesson 1
  - Lesson 2
```

## Search and Discovery

Plans are discoverable through:
1. **Full-text search**: Content and metadata
2. **Tag filtering**: By tags array
3. **Status queries**: Find all active plans
4. **Type filtering**: Architecture vs implementation
5. **Relationship traversal**: Find related plans
6. **Date ranges**: Created/updated within period

## Integration Points

### With Commands
- `smart-create` uses full schema
- `update` modifies metadata
- `review` shows status and progress
- `subsection` creates child plans

### With Workflows
- Workflow mode tracks current plan
- Progress updates trigger status changes
- Completion criteria checked automatically

### With Analytics
- Duration tracking for estimates
- Completion rate analysis
- Dependency impact assessment
- Plan velocity metrics

## Best Practices

1. **Always include**:
   - Clear objectives
   - Success criteria
   - Timeline estimates
   - Status tracking

2. **Keep updated**:
   - Progress percentage
   - Status transitions
   - Actual vs estimated

3. **Link liberally**:
   - Connect related plans
   - Reference implementations
   - Track dependencies

4. **Use tags wisely**:
   - Project identifiers
   - Technology areas
   - Team assignments
   - Priority levels

This schema provides a robust foundation for plan management while remaining flexible enough to accommodate various planning styles and methodologies.