---
type: documentation
category: sessions
tags: [org-mode, migration, content-analysis, categorization]
status: active
complexity: intermediate
priority: high
frequency: daily
audience: [flowloom-ai]
introduced_in: "FlowLoom 2.0.0"
last_verified: "2025-06-08"
maintainer: FlowLoom Core Team
permalink: content-migration-analysis
depends_on: [org-reorganization-status-assessment, unified-org-roam-gtd-system]
related_to: [display.md, org-mode-gtd-refactor-branch]
---

# Content Migration Analysis - 2025-06-08

## Source Content Overview

### Current Structure in org-files/
- **Total org files**: 315 files
- **Main GTD files**: gtd.org, inbox.org, someday.org, tickler.org, reference.org
- **Existing directories**: work/, personal/, projects/, roam/, roam-notes/, roam-archive/
- **Roam content**: Extensive timestamped notes with roam structure already partially in place

## Migration Strategy

### Phase 1: Core GTD Files ✅ READY
1. **gtd.org**: Contains work-focused content with time tracking - needs categorization
2. **inbox.org**: Recent captures, mixed content - analyze and distribute
3. **someday.org**: Personal someday items (Arrowverse watching) - move to personal/
4. **tickler.org**: Recurring items and scheduled tasks - distribute to contexts
5. **reference.org**: Currently empty - can be archived

### Phase 2: Existing Work/Personal Directories ✅ IDENTIFIED
- **work/**: Directory exists but empty
- **personal/**: Directory exists but empty  
- **projects/**: Directory exists but needs context separation

### Phase 3: Roam Content Migration ✅ ANALYZED
- **roam-notes/**: 315 timestamped org-roam notes (2022-2025)
  - Work-related: Django models, AtlasUp, technical notes
  - Personal: Learning, tools, general knowledge
  - Mixed: Programming concepts, methodologies
- **roam-archive/**: Archived project-specific notes (atlas_up_langchain_refactor)
- **roam/**: Existing structure with work/, personal/, areas/, resources/

## Content Categorization

### Work Context (@work)
- **Source**: Current gtd.org work section
- **Content**: Performance reviews, tickets (AUP-1247), time tracking
- **Target**: home/org/work/gtd.org
- **Roam notes**: Technical, Django, AtlasUp-related

### Personal Context (@personal)  
- **Source**: someday.org, personal captures
- **Content**: Arrowverse watching, personal learning
- **Target**: home/org/personal/
- **Roam notes**: General learning, personal tools

### Mixed/Reference
- **Source**: tickler.org (recurring items), reference materials
- **Content**: Yearly recurring tasks, general reference
- **Target**: Context-specific distribution or home/org/roam/resources/

## Migration Approach

### Step 1: Extract Work Content
```
gtd.org Work section → home/org/work/gtd.org
- Preserve time tracking structure
- Add @work tags  
- Keep performance review and ticket content
```

### Step 2: Extract Personal Content  
```
someday.org → home/org/personal/someday.org
tickler.org personal items → home/org/personal/gtd.org
- Add @personal tags
- Categorize recurring vs active items
```

### Step 3: Process Inbox
```
inbox.org → Distribute to:
- Work items → home/org/work/gtd.org
- Personal items → home/org/personal/gtd.org  
- Reference → home/org/roam/resources/
```

### Step 4: Migrate Roam Content
```
roam-notes/ → home/org/roam/
- Work-related → home/org/roam/work/
- Personal → home/org/roam/personal/
- General/Tools → home/org/roam/areas/ or resources/
- Preserve timestamp structure and roam IDs
```

### Step 5: Archive Completed
```
roam-archive/ → home/org/roam/work/ (if work-related)
Preserve as historical reference with clear archive status
```

## Implementation Plan

### Immediate Actions
1. **Migrate core GTD content** with proper context tags
2. **Distribute inbox items** to appropriate contexts  
3. **Categorize and move roam notes** preserving IDs and structure
4. **Update file references** and roam links
5. **Test agenda and roam functionality**

### Validation Steps
1. **Verify roam links** still work after migration
2. **Check agenda views** pick up new file structure
3. **Test capture templates** target correct locations
4. **Ensure time tracking** continues to function

## Risk Mitigation
- **Backup**: org-files/ remains as complete backup
- **Incremental**: Migrate in phases with testing
- **Preserve IDs**: Keep roam IDs and timestamps intact
- **Link validation**: Check roam graph integrity after migration

## Observations
- [structure] Existing partial work/personal separation provides good foundation
- [roam] Extensive roam content with proper IDs - preserve structure
- [gtd] Current GTD content is work-focused - needs personal context balance
- [migration] 315 files manageable with systematic approach

## Relations
- implements [[org-reorganization-status-assessment]]
- prepares_for [[Migration Execution Phase]]
- depends_on [[Unified Org-Roam GTD System]]