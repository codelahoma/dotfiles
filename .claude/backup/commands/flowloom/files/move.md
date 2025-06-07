Let input_args = "$ARGUMENTS"

## Argument Interpretation
First, analyze the provided arguments: input_args

Expected patterns:
- `source_path destination_path` - Move specific file or directory
- `pattern destination_path` - Move files matching pattern (e.g., "*.py src/")
- `bulk file1,file2,file3 destination_path` - Move multiple specific files
- `feature:name new_directory` - Move all files related to a feature

If input_args doesn't match these patterns, ask the user to clarify what they want to move and where.

## Safe File Moving Process

You should help the user move files safely while preserving git history and maintaining code functionality.

### Step 1: Safety Verification
@bash
git status

Show the user the git status to ensure we're starting with a clean working directory.

### Step 2: Move Analysis
Parse the input_args to understand:
- Source files or patterns to move
- Destination directory
- Whether destination exists or needs creation
- Type of move operation (single file, bulk, pattern-based)

Use Glob and LS tools to:
- Verify source files exist
- Check if destination directory exists
- Identify any potential conflicts
- Preview what will be moved

### Step 3: Dependency Check
Before moving files, analyze potential impacts:
- Search for import statements that reference the files
- Check for configuration files that might reference the paths
- Look for documentation or comments with file paths
- Identify any build scripts or tools that might be affected

Use Grep tool to search for file references in the codebase.

### Step 4: Safe Move Strategy
Create a move plan that:
- Uses `git mv` for tracked files to preserve history
- Creates destination directories if needed
- Groups related moves together
- Provides clear before/after structure
- Identifies files that will need import path updates

### Step 5: Execute Move Operations
If the user approves the plan:

@bash
# Create destination directory if needed
mkdir -p destination_path

For each file or group of files:

@bash
git mv source_file destination_path/

Show the user each move operation as it completes.

### Step 6: Update References
After moving files, help identify and update:
- Import statements that need new paths
- Configuration references
- Documentation that mentions file locations
- Any build or deployment scripts

### Step 7: Verification
@bash
git status

Show the user the final git status to confirm all moves completed successfully.

### Move Operation Guidelines
- Always use `git mv` for tracked files to preserve history
- Create destination directories before moving files
- Move related files together as logical groups
- Double-check that moved files are accessible
- Update import paths and references as needed
- Test that the project still functions after moves

### Common Move Patterns
- **Reorganizing features**: Moving all files related to a feature into a dedicated directory
- **Separating concerns**: Moving mixed file types into appropriate directories
- **Consolidating scattered files**: Gathering related files that are spread across directories
- **Framework migrations**: Moving files to match framework conventions

Focus on maintaining project functionality while improving organization and discoverability.