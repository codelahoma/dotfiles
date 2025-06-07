Show status and progress of all active FlowLoom plans

# Plan Status Overview

This command provides a comprehensive view of plan progress across the FlowLoom project.

## Step 1: Scan Recent Plans
Use the recent_plans utility to identify active plans, then analyze their status markers:

```bash
${FLOWLOOM_WORK_DIR:-.meta-claude}/bin/recent_plans
```

## Step 2: Analyze Plan Status
For each plan found, extract and categorize status indicators:

### Status Markers to Look For:
- **✅ COMPLETE** / **✅ IMPLEMENTATION COMPLETE** / **✅ PHASE COMPLETE** - Completed work
- **🔄 IN PROGRESS** - Currently active work
- **📝 PLANNED** - Future planned work  
- **⚠️ BLOCKED** - Blocked or dependent work
- **[x]** vs **[ ]** - Completed vs pending checklist items

### Progress Categories:
1. **Active Plans** - Plans with 🔄 IN PROGRESS markers or recent modifications
2. **Completed Plans** - Plans marked as ✅ COMPLETE
3. **Blocked Plans** - Plans with ⚠️ BLOCKED markers
4. **Planned Work** - Plans with 📝 PLANNED status

## Step 3: Memory Context
Check the memory graph for additional plan context:

```
Use mcp__memory__search_nodes with query "plan" to find plan-related entities
```

Look for:
- Plan entities and their current status
- Implementation progress observations
- Dependencies and blockers
- Recent activity related to plans

## Step 4: Generate Status Report
Present a structured overview:

### Format:
```
# FlowLoom Plan Status Report

## Currently Active (🔄 IN PROGRESS)
- Plan XXX: [Title] - [Progress summary]

## Recently Completed (✅ COMPLETE)  
- Plan XXX: [Title] - [Completion date/notes]

## Blocked/Waiting (⚠️ BLOCKED)
- Plan XXX: [Title] - [Blocking reason]

## Planned/Upcoming (📝 PLANNED)
- Plan XXX: [Title] - [Dependencies/timeline]

## Memory Graph Insights
[Any relevant context from memory search]

## Recommendations
[Suggested next steps based on current status]
```

## Step 5: Actionable Insights
Provide specific recommendations:
- Which plans need immediate attention
- What's blocking progress and how to resolve
- Logical next steps for active work
- Plans ready to transition to implementation

This gives a comprehensive view of where the project stands and what to focus on next.