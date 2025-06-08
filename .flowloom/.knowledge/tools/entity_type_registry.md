---
type: reference
category: tools
tags:
- memory
- entity-types
- consistency
- standards
status: active
depends_on:
- flowloom-memory
- log_observation.py
related_to:
- flowloom_memory_unified_interface
introduced_in: FlowLoom 2.0.0
last_verified: '2025-06-07'
complexity: beginner
stability: stable
priority: high
frequency: daily
audience:
- developers
- contributors
completeness: 100
needs_update: false
review_date: '2025-07-07'
maintainer: FlowLoom Core Team
permalink: tools/entity-type-registry
---

# FlowLoom Entity Type Registry

Standardized entity types for consistent knowledge graph organization. Use these canonical types to avoid fragmentation and ensure proper relationship mapping.

## Core Entity Types

### Development Workflow
- **Session** - Development or debugging sessions
- **Task** - Specific work items or todos
- **Feature** - Major functionality or capabilities
- **Component** - Software modules, classes, services
- **System** - Infrastructure, tools, environments

### Knowledge & Documentation
- **Documentation** - Guides, decisions, specifications
- **Architecture** - System design and structure decisions
- **TechnicalDebt** - Planned improvements and refactoring items
- **Troubleshooting** - Problem resolution and debugging guides
- **API** - Interface definitions and endpoint documentation

### Quality & Process
- **Test** - Test cases, suites, and validation procedures
- **Bug** - Issues, defects, and problem reports
- **Review** - Code reviews, design reviews, assessments
- **Deployment** - Release processes and environment management
- **Monitoring** - Observability and health tracking

### People & Organization
- **User** - End users, personas, stakeholders
- **Developer** - Team members and contributors
- **Team** - Groups and organizational units
- **Stakeholder** - Business and external parties

### External Integration
- **Service** - External services and dependencies
- **Tool** - Development tools and utilities
- **Configuration** - Settings, configs, and parameters
- **Environment** - Development, staging, production contexts

## Usage Guidelines

### Before Creating Entities

1. **Check existing types** - Use this registry first
2. **Search for similar entities** - `flowloom-memory search "keyword"`
3. **Use canonical names** - Prefer established types over new ones
4. **Consider relationships** - How will this connect to existing entities?

### Type Selection Rules

**Use specific types over generic ones**:
- ✅ `Feature` instead of `Thing`
- ✅ `Component` instead of `Code`
- ✅ `Session` instead of `Activity`

**Be consistent with naming patterns**:
- ✅ `PascalCase` for types
- ✅ `Descriptive-Kebab-Case` for entity names
- ✅ `lowercase` for relation types

**Consider lifecycle and evolution**:
- `Task` → `Feature` (when task becomes larger)
- `Bug` → `TechnicalDebt` (when bug becomes improvement)
- `Documentation` → `Architecture` (when docs become design)

## Common Anti-Patterns to Avoid

### Type Fragmentation
❌ **Bad**: `Development`, `Coding`, `Programming`, `Work`
✅ **Good**: `Session` (for all development activities)

❌ **Bad**: `Issue`, `Problem`, `Error`, `Defect`
✅ **Good**: `Bug` (for all defects)

❌ **Bad**: `Docs`, `Guide`, `Manual`, `Reference`
✅ **Good**: `Documentation` (with different relation types)

### Overly Generic Types
❌ **Bad**: `Item`, `Thing`, `Object`, `Data`
✅ **Good**: Use specific types that describe actual purpose

### Implementation-Specific Types
❌ **Bad**: `PythonClass`, `JavaMethod`, `ReactComponent`
✅ **Good**: `Component` (with language/framework in entity name)

## Type Evolution Guidelines

### When to Add New Types
- **Clear semantic distinction** from existing types
- **Multiple entities** would benefit from the new type
- **Specific relationships** that don't fit existing patterns
- **Domain expertise** requires specialized categorization

### When to Merge Types
- **Overlapping semantics** between existing types
- **Confusion** about which type to use
- **Limited relationship value** from having separate types
- **Maintenance burden** of keeping types distinct

## Validation Commands

### Check Type Consistency
```bash
# List all entity types in use
flowloom-memory query "SELECT type, COUNT(*) as count FROM entities GROUP BY type ORDER BY count DESC"

# Find potential duplicates
flowloom-memory search "authentication" | head -10

# Analyze relationship patterns
flowloom-memory query "SELECT relation_type, COUNT(*) as count FROM relations GROUP BY relation_type ORDER BY count DESC"
```

### Clean Up Inconsistencies
```bash
# Find entities that might need type updates
flowloom-memory query "SELECT name, type FROM entities WHERE type NOT IN ('Session', 'Task', 'Feature', 'Component', 'System', 'Documentation', 'Architecture', 'TechnicalDebt', 'Bug', 'User', 'Service', 'Tool', 'Configuration')"

# Review and update as needed
flowloom-memory get-entity "Entity-Name-Here"
```

## Relationship Type Standards

### Common Relation Types
- `depends_on` - Dependency relationships
- `implements` - Implementation relationships
- `contains` - Containment/composition
- `relates_to` - General associations
- `creates` - Creation relationships
- `uses` - Usage relationships
- `tests` - Testing relationships
- `documents` - Documentation relationships

## Type Registry Maintenance

### Review Schedule
- **Weekly**: Check for new types being introduced
- **Monthly**: Analyze type usage patterns and consolidation opportunities
- **Quarterly**: Full registry review and cleanup

### Process for Type Changes
1. **Propose change** in team discussion
2. **Create migration plan** for existing entities
3. **Update registry** with new standards
4. **Migrate existing data** using batch operations
5. **Update documentation** and examples

### Tools for Maintenance
```bash
# Generate type usage report
flowloom-memory stats

# Export entity data for analysis
flowloom-memory query "SELECT * FROM entities" -o csv > entity_analysis.csv

# Find orphaned or underused types
flowloom-memory query "SELECT type, COUNT(*) as count FROM entities GROUP BY type HAVING count = 1"
```

## Implementation Notes

This registry should be:
- **Referenced** before creating any new entities
- **Updated** when new patterns emerge
- **Enforced** through code review and tooling
- **Evolved** based on actual usage patterns

The goal is consistency without rigidity - the registry should serve the development process, not constrain it unnecessarily.