Let input_args = "$ARGUMENTS"

Reorganize your experimental changes into logical commits for a clean project history.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the argument pattern:
- If input_args is empty: Analyze all unstaged and untracked changes
- If input_args contains a commit hash: Start from that specific commit point
- If input_args contains "auto": Automatically execute the suggested commit plan
- If input_args contains "dry-run": Show the commit plan without executing

## Argument Patterns
- (no arguments) - Analyze current unstaged/untracked changes
- `abc123def` - Start analysis from specific commit hash
- `auto` - Automatically execute the suggested commit plan
- `dry-run` - Show commit plan without executing
- `auto abc123def` - Auto-execute starting from specific commit

Examine the current project state (unstaged and untracked files) and:

1. Group the changes into logical, coherent commits that would make sense to a developer reviewing the history
2. Consider dependencies between files when grouping
3. Prioritize commits that implement complete features or fixes
4. For each proposed commit:
   - Suggest a clear, descriptive commit message following best practices
   - Explain briefly why these files belong together
   - List the specific files to include
5. After proposing the groupings, execute the git commands to stage and commit each group in the correct order

Some guidelines:
- Configuration and dependency changes should be separate commits
- Tests should be committed with their implementation
- Prefer smaller, focused commits over large ones
- Use conventional commit format (type: subject) where appropriate
- Structure each commit to pass tests if possible

Once you've laid out the plan, wait for confirmation before attempting any commits (unless \"auto\" argument was provided).