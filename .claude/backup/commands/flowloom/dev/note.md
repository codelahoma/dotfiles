Let input_args = "$ARGUMENTS"

You are helping the user capture development notes and insights directly into FlowLoom's knowledge systems during the development adventure.

## Argument Interpretation
First, analyze the provided arguments: input_args

The input_args contains the note content that should be captured in FlowLoom's knowledge systems as being related to this development adventure.

## Development Note Capture Process

1. **Parse the Note Content**
   Extract the note content from input_args and identify key elements:
   - Main insight or observation
   - Technical details or decisions
   - Context (what prompted this note)
   - Relevance to FlowLoom development

2. **Multi-System Capture**
   Store the note in multiple knowledge systems for comprehensive capture:
   
   **Memory Graph Integration**:
   - Use mcp__memory__create_entities to add note as entity with type "development_note"
   - Use mcp__memory__create_relations to link to relevant FlowLoom concepts
   - Use mcp__memory__add_observations to capture the insight in context

   **Basic Memory System**:
   - Use mcp__basic-memory__write_note to store in "FlowLoom Development" folder
   - Include timestamp and development session context
   - Tag with relevant categories (architecture, bootstrap, configuration, etc.)

   **SQLite Tracking**:
   - Use mcp__sqlite__write_query to log the note with metadata
   - Store timestamp, session context, and note category
   - Enable future analytics on development insights

   **Story Integration**:
   - Consider if this note should be added to "The Story of FlowLoom.md"
   - Capture in story mode format if it represents a significant insight

3. **Context Enhancement**
   Enrich the note with:
   - Current development session context
   - Related git commits or recent changes
   - Connections to existing FlowLoom concepts
   - Potential impact on future development

4. **Confirmation**
   Provide confirmation of where the note was captured and how it can be retrieved later.

The goal is to ensure no development insights are lost and that they're stored in multiple, searchable formats for future reference during FlowLoom's evolution.

Show the user what was captured and where it's stored for future reference.