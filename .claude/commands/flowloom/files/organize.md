Organize and restructure project files and directories intelligently
Let input_args = "$ARGUMENTS"

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Analyze entire project structure and suggest organization improvements
- If input_args is a directory path: Focus organization efforts on that specific directory
- If input_args is a file type (e.g., "*.py", "config", "docs"): Organize files of that type
- If input_args is "scattered": Find and organize scattered files that should be grouped

## File Organization Process

You should help the user organize their project files systematically and safely.

### Step 1: Analysis Phase
@bash
git status

Show the user the git status to ensure we're starting with a clean working directory.

### Step 2: Structure Assessment
Analyze the current project structure in the specified scope (input_args). Use the LS and Glob tools to:
- Examine directory hierarchies
- Identify files that seem misplaced
- Look for patterns in file organization
- Find groups of related files that could be better organized

### Step 3: Organization Recommendations
Based on your analysis, provide specific recommendations:
- Files that should be moved to different directories
- New directories that should be created for better organization
- Naming convention improvements
- Grouping opportunities for related files

### Step 4: Safe Implementation Plan
Create a step-by-step plan for the organization that:
- Uses `git mv` for tracked files to preserve history
- Groups related moves together logically
- Provides rollback options if needed
- Minimizes disruption to existing functionality

### Step 5: Execute Organization
If the user approves your plan, execute the file moves systematically:
- Create new directories as needed
- Move files using appropriate git commands
- Update any obvious import paths or references
- Verify the moves completed successfully

### Safety Guidelines
- Always check git status before starting
- Use git mv for tracked files
- Move files in logical groups
- Test that important files are still accessible after moves
- Provide clear before/after documentation

Focus on creating a cleaner, more logical project structure that will be easier to navigate and maintain long-term.