---
title: GTD Implementation - Project Context and Boundaries
type: note
permalink: architecture/gtd-implementation-project-context-and-boundaries
---

# GTD Implementation - Project Context and Boundaries

## Critical Distinction: Greenfield vs Previous Attempts

This document establishes clear boundaries between our current greenfield GTD implementation and previous attempts.

## Current Greenfield Implementation (2025-06)

### Location of Assets
- **Research**: `.flowloom/.knowledge/research/`
  - Org-Mode GTD Architecture Best Practices
  - Elisp Fundamentals for Org-Mode Customization
  - Org-Mode Capture Templates Research
  - Spacemacs Keybinding System Research
  - Org-Babel Literate Programming Research
  - GTD Package Analysis and Best Practices

- **Architecture**: `.flowloom/.knowledge/architecture/`
  - GTD System Architectural Synthesis
  - GTD Implementation - Project Context and Boundaries (this doc)

- **Plans**: `.flowloom/.knowledge/plans/`
  - GTD System Implementation - High-Level Plan
  - `.flowloom/.knowledge/plans/gtd-implementation/`
    - GTD Implementation Plan - 100 Foundation Setup
    - GTD Implementation Plan - 110 Core GTD Engine
    - GTD Implementation Plan - 120 Keybinding System
    - (Additional component plans to be added)

### Key Characteristics
- **Approach**: Literate programming using org-babel
- **Architecture**: Component-based with progressive enhancement
- **Keybindings**: `SPC o o` namespace
- **Philosophy**: "Progressive Enhancement through Literate Programming"
- **Implementation File**: `~/org/gtd-system.org` (to be created)

## Previous Attempts (To Avoid Confusion With)

### Location of Previous Work
- **Plans Directory**: `home/plans/`
  - Contains older GTD planning documents
  - Phase-based implementation approaches
  - Different architectural decisions

- **Existing Config**: `home/.spacemacs.d/codelahoma-org.el`
  - Contains previous GTD capture templates
  - Different keybinding schemes
  - To be replaced/superseded by new implementation

### Key Differences from Previous Attempts
1. **Documentation Location**: Knowledge base vs plans directory
2. **Implementation Method**: Literate programming vs direct elisp
3. **Architecture**: Component-based vs monolithic
4. **Keybinding Namespace**: `SPC o o` vs various schemes
5. **File Organization**: Enhanced structure with performance optimizations

## Working Guidelines

### During Implementation
1. **Always reference knowledge base docs**, not old plans directory
2. **Implement in `gtd-system.org`**, not directly in config files
3. **Use component numbers** (100, 110, 120) to track progress
4. **Follow architectural synthesis** decisions, not old patterns

### File References
- ✅ **Correct**: `.flowloom/.knowledge/plans/gtd-implementation/`
- ❌ **Incorrect**: `home/plans/`
- ✅ **Correct**: `.flowloom/.knowledge/architecture/GTD System Architectural Synthesis.md`
- ❌ **Incorrect**: Previous unified system docs

### Component Tracking
When implementing, reference components by their numbers:
- 100-109: Foundation Setup
- 110-119: Core GTD Engine
- 120-129: Keybinding System
- 130-139: Capture System
- 140-149: Processing Workflow
- 150-159: Review System
- 160-169: Context Engine
- 170-179: Integration Layer
- 180-189: Testing Framework
- 190-199: Documentation & Migration

## Implementation Status Tracking

### Planned Components
- [x] 100: Foundation Setup (plan created)
- [x] 110: Core GTD Engine (plan created)
- [x] 120: Keybinding System (plan created)
- [ ] 130: Capture System
- [ ] 140: Processing Workflow
- [ ] 150: Review System
- [ ] 160: Context Engine
- [ ] 170: Integration Layer
- [ ] 180: Testing Framework
- [ ] 190: Documentation & Migration

### Implementation Progress
- [ ] Create `~/org/gtd-system.org`
- [ ] Implement Component 100
- [ ] Implement Component 110
- [ ] Implement Component 120
- [ ] (Continue with remaining components)

## Notes for FlowLoom

When working on GTD implementation:
1. This is a **greenfield project** - ignore previous implementations
2. All documentation is in **knowledge base**, not plans directory
3. Implementation uses **literate programming** in org-babel
4. Follow the **component numbering** system strictly
5. Reference the **architectural synthesis** for all decisions

---

*This document ensures clear separation between current greenfield GTD implementation and previous attempts, preventing confusion during development.*