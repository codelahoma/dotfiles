Let input_args = "$ARGUMENTS"

Run the linting workflow for your specified files or all staged files if none are specified.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the argument pattern:
- If input_args is empty: Check staged files and run linting on those
- If input_args contains file paths: Run linting on those specific files
- If input_args contains "all": Run linting on all Python files in the project
- If input_args contains "changed": Run linting on all modified files (staged + unstaged)

## Argument Patterns
- (no arguments) - Lint all currently staged files
- `file1.py file2.py` - Lint specific files
- `all` - Lint all Python files in project
- `changed` - Lint all modified files (staged and unstaged)
- `atlas_up/` - Lint all files in specific directory

Follow these steps:

1. Run script/run_pre_commit_no_mypy on the determined files
2. If formatters modified files, stage the modifications
3. If linters found errors, fix them one by one
4. Continue this process until all linting checks pass
5. Confirm all changes are ready to commit once linting passes
