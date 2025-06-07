Let input_args = "$ARGUMENTS"

Find and remove all backup branches for a specific branch.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the argument pattern:
- If input_args is empty: Clean up backups for your current branch
- If input_args contains a branch name: Clean up backups for that specific branch
- If input_args contains "all": Show all backup branches across all branches
- If input_args contains "force": Skip confirmation prompts and delete automatically

## Argument Patterns
- (no arguments) - Clean up backups for current branch
- `feature-branch` - Clean up backups for specific branch
- `all` - Show all backup branches across all branches
- `force` - Skip confirmation and delete automatically
- `feature-branch force` - Clean specific branch backups without confirmation

This command helps you clean up backup branches created with the `/project:git:backup` command.

If you provide a branch name in input_args, it will find and remove all backups for that branch.
If you don't provide a branch name, it will use your current branch.

The command will:

1. Determine the target branch name (from your input or current branch)
2. Find all backup branches matching the pattern `backup/<target-branch>/*`
3. Show you a list of found backup branches with their creation dates
4. Ask for your confirmation before deletion
5. Delete all the backup branches
6. Show a confirmation of the deleted branches

```bash
# Determine target branch
TARGET_BRANCH=""
if [ -n "$input_args" ]; then
  TARGET_BRANCH="$input_args"
else
  TARGET_BRANCH=$(git branch --show-current)
fi

echo "Finding backup branches for: $TARGET_BRANCH"

# Find all backup branches for this branch
BACKUP_PATTERN="backup/$TARGET_BRANCH/"
BACKUP_BRANCHES=$(git branch | grep "$BACKUP_PATTERN" | sed 's/^[ *]*//')

# Check if any backup branches were found
if [ -z "$BACKUP_BRANCHES" ]; then
  echo "No backup branches found for '$TARGET_BRANCH'."
  exit 0
fi

# Display the backup branches with creation dates
echo "Found the following backup branches:"
echo ""

for branch in $BACKUP_BRANCHES; do
  # Get creation date of branch (commit date of first commit in branch)
  CREATION_DATE=$(git show --format="%ci" $branch | head -n 1)
  echo "- $branch (created: $CREATION_DATE)"
done

# Ask for confirmation
echo ""
echo "Do you want to delete all these backup branches? (yes/no)"
```

After confirmation, the command will:
```bash
# Delete all backup branches
for branch in $BACKUP_BRANCHES; do
  git branch -D "$branch"
  echo "Deleted: $branch"
done

# If any were pushed to remote, offer to delete remote branches
HAS_REMOTE=$(git branch -r | grep "$BACKUP_PATTERN")
if [ -n "$HAS_REMOTE" ]; then
  echo ""
  echo "Some of these branches may exist on the remote."
  echo "Do you want to delete them from the remote as well? (yes/no)"
fi
```

If you choose to delete remote branches, the command will:
```bash
for branch in $BACKUP_BRANCHES; do
  REMOTE_BRANCH=${branch#*/}  # Remove 'backup/' prefix
  git push origin --delete "$REMOTE_BRANCH"
  echo "Deleted remote branch: $REMOTE_BRANCH"
done
```

This helps you keep your repository clean by removing temporary backup branches when they're no longer needed.