---
title: FlowLoom Memory Entity Naming System
type: note
permalink: technical/flow-loom-memory-entity-naming-system
---

# FlowLoom Memory Entity Naming System

## Current State Analysis

The root memory.json contains only 6 entities with clean naming:
- FlowLoom (Project)
- FlowLoom Product Specification (Document) 
- FlowLoom Command System (Feature)
- FlowLoom Plan Management (Feature)
- FlowLoom Configuration (Feature)
- Repository Cleanup Task (Task)

Current entity types: Project, Document, Feature, Task

## Problem Statement

Current entity creation is ad-hoc, leading to:
- Inconsistent naming patterns
- Duplicate entities for same concepts
- Search failures due to name variations
- Lack of hierarchical organization

## Proposed Entity Naming Convention

### Hierarchical Naming Structure

```
[Scope]:[Component]:[SubComponent]:[Type]
```

Examples:
- `FlowLoom:Core:CommandSystem:Feature`
- `FlowLoom:Memory:EntityNaming:System`
- `FlowLoom:Docker:Environment:Component`
- `FlowLoom:Session:Management:System`

### Naming Rules

1. **Scope Prefixes**:
   - `FlowLoom:` - Core FlowLoom functionality
   - `Project:` - User project-specific entities
   - `Session:` - Session-specific tracking
   - `External:` - Third-party integrations

2. **Component Categories**:
   - `Core` - Fundamental systems (commands, memory, config)
   - `Infrastructure` - Docker, installers, session management
   - `Features` - User-facing functionality
   - `Integration` - External tool connections
   - `Documentation` - Plans, guides, specifications

3. **Naming Conventions**:
   - Use PascalCase for all name segments
   - Avoid abbreviations (use `CommandSystem` not `CmdSys`)
   - Be specific but concise
   - Use consistent terminology across related entities

### Entity Type Standardization

```yaml
# Core Types
Project: Top-level project or initiative
System: Complex multi-component functionality
Component: Major architectural piece
Feature: User-facing capability
Service: Background/supporting functionality

# Documentation Types  
Document: Static documentation (plans, specs)
Guide: How-to documentation
Decision: Architectural decision record

# Development Types
Task: Specific work item
Issue: Problem to be solved
Epic: Large feature development

# Tracking Types
Session: Development session
Milestone: Progress checkpoint
Release: Version or deployment
```

## Entity Resolution Algorithm

### Before Creating New Entity

1. **Canonical Name Generation**:
   ```
   Input: "flowloom docker component"
   Canonical: "FlowLoom:Infrastructure:Docker:Component"
   ```

2. **Search Strategy**:
   - Exact canonical match
   - Fuzzy match on canonical form
   - Search aliases (see aliasing system)
   - Search by keywords in observations

3. **Match Confidence Scoring**:
   - 100%: Exact canonical match
   - 90%: Same canonical with different casing
   - 80%: Alias match
   - 70%: High keyword overlap in observations
   - <70%: Suggest new entity creation

### Implementation Functions

```javascript
function generateCanonicalName(userInput, entityType) {
  // Parse natural language input
  // Apply naming conventions
  // Return standardized name
}

function findExistingEntity(canonicalName) {
  // Search exact matches
  // Search aliases  
  // Fuzzy search with scoring
  // Return best matches with confidence
}

function createEntitySafely(name, type, observations) {
  // Check for existing entity
  // Offer merge if high confidence match
  // Create with canonical name if no match
}
```

## Entity Aliasing System

### Alias Types

1. **Legacy Names**: Previous naming conventions
2. **Natural Language**: Common ways users refer to entities
3. **Abbreviations**: Short forms for frequent use
4. **Synonyms**: Alternative terms for same concept

### Alias Storage

```json
{
  "name": "FlowLoom:Infrastructure:Docker:Component",
  "entityType": "Component",
  "aliases": [
    "FlowLoom Docker Component",
    "Docker Environment", 
    "FlowLoom Docker System",
    "FLDocker"
  ],
  "observations": [...]
}
```

## Migration Strategy

### Phase 1: Schema Enhancement
- Add alias support to memory system
- Implement canonical name generation
- Create entity resolution functions

### Phase 2: Existing Entity Analysis  
- Scan all memory.json files for naming patterns
- Identify potential duplicates
- Generate canonical names for existing entities

### Phase 3: Interactive Migration
- Present potential duplicates to user
- Allow merge or keep separate decisions
- Update entity names to canonical form
- Preserve original names as aliases

### Phase 4: Integration
- Update all entity creation points to use new system
- Add validation to prevent non-canonical names
- Create CLI tools for entity management

## Implementation Priority

1. **High Priority**:
   - Canonical name generation
   - Basic entity resolution
   - Duplicate detection

2. **Medium Priority**:
   - Alias system
   - Interactive migration tool
   - CLI entity management

3. **Low Priority**:
   - Advanced fuzzy matching
   - Automatic categorization
   - Cross-session entity linking

## Benefits

- **Consistency**: Predictable naming patterns
- **Searchability**: Easy to find existing entities
- **Scalability**: Hierarchical organization
- **User Experience**: Less duplicate creation frustration
- **Maintenance**: Easier to manage large memory graphs

## Next Steps

1. Design canonical name generation algorithm
2. Implement entity resolution functions
3. Create migration tool for existing entities
4. Integrate into memory creation workflows