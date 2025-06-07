Let input_args = "$ARGUMENTS"

You are helping the user create a comprehensive project plan using MCP-enhanced capabilities for research, file management, and knowledge tracking.

## Argument Interpretation
First, analyze the provided arguments: input_args

**When no arguments provided (input_args is empty):**
Offer the user these options:
1. **Use current context** - Create plan based on current session context with smart research
2. **Specify plan topic** - Enter what you want to plan (e.g., "API integration", "testing framework")
3. **Research-driven planning** - Let AI analyze project and suggest what to plan
4. **Guided plan creation** - Interactive conversation to determine scope and focus

Present these options clearly and wait for user choice.

**When arguments provided:**
Expected pattern: `[plan_name] [optional: keywords or focus areas]`
- Plan name only: Create general plan with research
- Plan name + keywords: Focus research and planning on those areas

## User Interaction Process

When input_args is empty, present this menu:
```
Smart Plan Creation with Research

Choose your approach:
1. Use current context (based on: [describe current session context])
2. Specify plan topic
3. Research-driven planning (AI suggests topics)
4. Guided plan creation

What would you like to do? (1-4): 
```

Then proceed based on user selection.

## Smart Plan Creation Process

1. **Research Phase (Using MCP Capabilities)**
   - Use mcp__brave-search__brave_web_search to research current best practices for the plan topic
   - Use mcp__filesystem__search_files to find related existing plans or documentation
   - Use mcp__memory__search_nodes to find relevant past knowledge and context
   - Use mcp__basic-memory__search_notes to discover related notes and discussions

2. **Context Building**
   - Use mcp__git__git_log to understand recent project changes that might affect planning
   - Use mcp__filesystem__directory_tree to understand current project structure
   - Use mcp__github__search_repositories (if applicable) to find similar projects for reference

3. **Plan Document Creation**
   - Create plan using the hierarchical numbering system from CLAUDE.local.md
   - Structure plan with research-backed sections
   - Include references to external sources found during research
   - Store plan in appropriate plans/PROJECT_NAME/ directory

4. **Knowledge Graph Integration**
   - Use mcp__memory__create_entities to add plan entities to knowledge graph
   - Use mcp__memory__create_relations to link plan to relevant project concepts
   - Use mcp__memory__add_observations to capture key insights and decisions

5. **Database Tracking** 
   - Use mcp__sqlite__create_table (if needed) to set up plan tracking
   - Use mcp__sqlite__write_query to log plan creation and key metadata

6. **Follow-up Actions**
   - Use mcp__basic-memory__write_note to create implementation reminders
   - Suggest related commands for plan execution and monitoring

The result should be a well-researched, contextually-aware plan that leverages all available information sources and integrates with the project's knowledge management systems.

After creating the plan, provide a summary of:
- Research sources consulted
- Key insights discovered
- Knowledge graph additions made
- Next steps for plan execution