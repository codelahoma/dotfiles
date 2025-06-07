Display quick reference cheatsheet of available Claude commands
Let input_args = "$ARGUMENTS"

Display a quick reference cheatsheet of available Claude commands.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the argument pattern:
- If input_args is empty: Show full cheatsheet
- If input_args contains a category name: Show only that category
- If input_args contains "update": Regenerate the cheatsheet (for maintenance)

## Argument Patterns
- (no arguments) - Show complete command cheatsheet
- `docs` - Show only documentation commands
- `impl` - Show only implementation commands  
- `plan` - Show only planning commands
- `git` - Show only git commands
- `mode` - Show only mode commands
- `rev` - Show only review commands
- `memory` - Show only knowledge graph memory commands

## Display Process

Output the cheatsheet directly in your response text (not through tools):

If input_args is empty: Display the complete contents of `${FLOWLOOM_WORK_DIR:-.meta-claude}/docs/commands-cheatsheet.md` directly in your response
If input_args contains a category: Display only that section from the cheatsheet

This provides immediate visibility without requiring users to expand tool outputs.