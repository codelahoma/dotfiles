Let input_args = "$ARGUMENTS"

# FlowLoom Memory & Knowledge Management

FlowLoom uses a comprehensive dual memory system for project knowledge management. This command helps you access, update, and leverage all available memory tools.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Show full memory system overview and available operations
- If input_args contains "graph" or "entities": Focus on structured knowledge graph operations
- If input_args contains "notes" or "docs": Focus on narrative documentation operations  
- If input_args contains "session": Focus on session tracking and management
- If input_args contains "search": Perform cross-system knowledge search
- If input_args contains "status": Show current memory system status

## Dual Memory System Overview

### 1. Structured Knowledge Graph (memory.json)
**Use for:** Facts, relationships, status tracking, project components

**Key Operations:**
- `mcp__memory__create_entities` - Create trackable components, features, plans
- `mcp__memory__create_relations` - Link entities with relationships
- `mcp__memory__add_observations` - Add facts and status updates
- `mcp__memory__search_nodes` - Find entities and relationships
- `mcp__memory__open_nodes` - Get detailed entity information
- `mcp__memory__read_graph` - View entire knowledge structure

**Entity Types:**
- **Components**: System parts (e.g., "FlowLoom Session Manager")
- **Features**: Specific functionality (e.g., "Git Command Permissions")
- **Plans**: Implementation roadmaps (e.g., "Plan 5120 Implementation")
- **Sessions**: Development work periods with outcomes
- **Decisions**: Architectural choices and rationale
- **Issues**: Problems, bugs, or blockers identified

### 2. Narrative Knowledge (.knowledge/)
**Use for:** Explanations, guides, design decisions, team wisdom

**Key Operations:**
- `mcp__basic-memory__write_note` - Create/update documentation articles
- `mcp__basic-memory__search_notes` - Find documentation by content
- `mcp__basic-memory__read_note` - Read specific documentation
- `mcp__basic-memory__build_context` - Follow up on previous discussions
- `mcp__basic-memory__recent_activity` - See recent knowledge activity

**Content Categories:**
- **Technical Decisions**: Architecture choices and rationale
- **Implementation Guides**: How-to documentation and patterns
- **Troubleshooting**: Problem solutions and recovery procedures
- **Development Notes**: Insights and lessons learned

## Memory Command Integration

### FlowLoom Memory Commands
- `/flowloom:memory:track-progress` - Record development progress
- `/flowloom:memory:review-context` - Review project context at session start
- `/flowloom:memory:update-status <entity> <status>` - Update component status
- `/flowloom:memory:create-session [description]` - Create session tracking

### Session Management Integration
Session tracking automatically creates knowledge graph entities and can generate narrative summaries of work sessions.

## Recommended Memory Workflow

### Starting Work Sessions
1. Use `/flowloom:memory:review-context` to understand current state
2. Create session entity with `/flowloom:memory:create-session` 
3. Search relevant knowledge with `mcp__basic-memory__search_notes`

### During Development
1. Track progress with `/flowloom:memory:track-progress`
2. Update entity status with `/flowloom:memory:update-status`
3. Document decisions with `mcp__basic-memory__write_note`

### Completing Work
1. Add final observations to relevant entities
2. Create cross-references between knowledge systems
3. Update relationships to reflect new dependencies

## Cross-System Integration Pattern

1. **Create entities** in knowledge graph for trackable items
2. **Write articles** in basic-memory for detailed explanations
3. **Cross-reference** by mentioning entity names in articles
4. **Search both** systems when looking for information

## Memory System Status

To check current memory system health:
- Review recent entities: `mcp__memory__search_nodes`
- Check recent documentation: `mcp__basic-memory__recent_activity`
- Validate entity relationships: `mcp__memory__read_graph`

This dual approach ensures both structured tracking and rich narrative context for comprehensive project knowledge management.