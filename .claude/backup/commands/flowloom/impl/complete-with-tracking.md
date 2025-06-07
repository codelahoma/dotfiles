Let input_args = "$ARGUMENTS"

# Complete Implementation with Knowledge Graph Tracking

Complete a development task and automatically update the knowledge graph with progress and outcomes.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Complete current task and auto-track progress
- If input_args is text: Use as completion description/summary
- If input_args starts with "component:": Complete work on specific component
- If input_args starts with "feature:": Complete work on specific feature

## Argument Patterns
- (no arguments) - Auto-detect completed work and track
- `implemented directory selection feature` - Completion description
- `component:installer phase-1 completed` - Component completion
- `feature:directory-selection testing complete` - Feature completion

## Instructions

1. **Identify what was completed** from input_args and current working context
2. **Check git status and recent commits** to understand what changed
3. **Run implementation completion tasks**:
   - Check for linting/formatting issues
   - Run tests if available
   - Validate implementation completeness
   - Create/update documentation if needed

4. **Update knowledge graph with completion**:
   - Mark relevant entities as completed or advanced in status
   - Record key implementation details and decisions
   - Note any new dependencies or requirements discovered
   - Document next steps or follow-up work needed

5. **Create git commit if appropriate**:
   - Stage relevant changes
   - Generate descriptive commit message
   - Include knowledge graph tracking in commit

## Knowledge Graph Updates
Automatically update entities with:
- **Status progression**: Mark phases/components as completed
- **Implementation notes**: Key decisions and approaches used
- **File changes**: Important files created or modified
- **Dependencies discovered**: New requirements or blockers found
- **Next steps**: Follow-up work or dependencies to address

## Output Format
After completion:
- Summarize what was completed
- Show knowledge graph updates made
- List any issues found that need follow-up
- Recommend next development priorities
- Confirm git commit if made

This command integrates completion workflows with knowledge graph maintenance for comprehensive project tracking.