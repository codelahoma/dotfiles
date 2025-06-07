Let input_args = "$ARGUMENTS"

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Perform comprehensive project cleanup analysis
- If input_args is "temp": Focus on temporary files and build artifacts
- If input_args is "obsolete": Find and remove obsolete code and files
- If input_args is "duplicates": Identify and consolidate duplicate files
- If input_args is a directory path: Clean up that specific directory

## Project Cleanup Process

You should help the user identify and safely remove unnecessary files while preserving important project assets.

### Step 1: Safety Check
@bash
git status

Show the user the git status to ensure we have a clean starting point.

### Step 2: Cleanup Analysis
Based on the input_args scope, systematically identify:

**Temporary Files:**
- Build artifacts and compilation outputs
- Cache files and temporary directories
- IDE-specific files that shouldn't be committed
- Log files and debug outputs

**Obsolete Files:**
- Old backup files and unused copies
- Deprecated code files no longer referenced
- Legacy configuration files
- Outdated documentation or comments

**Duplicate Content:**
- Files with similar names that might be duplicates
- Multiple versions of the same configuration
- Redundant documentation or README files

### Step 3: Safe Removal Plan
Create a systematic removal plan that:
- Identifies files safe to delete immediately
- Flags files that need review before deletion
- Preserves any files that might be referenced elsewhere
- Uses appropriate git commands for tracked vs untracked files

### Step 4: Cleanup Categories
Organize findings into clear categories:

**Safe to Remove:**
- Confirmed temporary files
- Build outputs that can be regenerated
- Files explicitly listed in .gitignore

**Needs Review:**
- Files that might be referenced by other code
- Configuration files that might be environment-specific
- Documentation that might still be relevant

**Keep but Organize:**
- Important files in wrong locations
- Files that need better naming or categorization

### Step 5: Execute Cleanup
If the user approves your plan:
- Remove safe files using appropriate commands
- Move files that should be relocated
- Update .gitignore if needed for future prevention
- Document what was cleaned up

### Cleanup Guidelines
- Never delete files that are actively referenced in code
- Check for import statements or config references before removing
- Be conservative with configuration files
- Document cleanup actions for potential rollback
- Focus on improving project maintainability

The goal is a cleaner, more maintainable project structure without any loss of important functionality or documentation.