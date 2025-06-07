Let input_args = "$ARGUMENTS"

Create a backup of your current git branch with a timestamp and optional message.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the argument pattern:
- If input_args is empty: Create backup with timestamp only
- If input_args contains text: Use that text as the backup message/description
- If input_args contains "push": Automatically push the backup to remote
- If input_args contains "quiet": Create backup without confirmation prompts

## Argument Patterns
- (no arguments) - Create timestamped backup
- `"working on feature X"` - Create backup with descriptive message
- `push` - Create backup and automatically push to remote
- `quiet` - Create backup without prompts
- `"feature checkpoint" push` - Create backup with message and push

This command will help you create a safe backup of your current branch by:

1. Determining your current branch name
2. Creating a timestamp in YYYY-MM-DD-HHMM format
3. Creating a backup branch with format: `backup/<your-branch-name>/<timestamp>`
4. Including any message you provided in the branch description
5. Offering to push the backup branch to remote
6. Reporting the created backup branch name

```bash
# Get current branch name
CURRENT_BRANCH=$(git branch --show-current)
echo "Current branch: $CURRENT_BRANCH"

# Create timestamp
TIMESTAMP=$(date +"%Y-%m-%d-%H%M")

# Create backup branch name
BACKUP_BRANCH="backup/$CURRENT_BRANCH/$TIMESTAMP"

# Check if a message was provided
MESSAGE=""
if [ -n "$input_args" ]; then
  MESSAGE=" - $input_args"
fi

# Create the backup branch
echo "Creating backup branch: $BACKUP_BRANCH$MESSAGE"
git checkout -b "$BACKUP_BRANCH"

# Add description if message provided
if [ -n "$MESSAGE" ]; then
  echo "Adding description: $MESSAGE"
  git config branch."$BACKUP_BRANCH".description "$input_args"
fi

# Return to original branch
git checkout "$CURRENT_BRANCH"
echo "✅ Successfully created backup branch: $BACKUP_BRANCH"
echo "   Return to this backup with: git checkout $BACKUP_BRANCH"

# Offer to push the backup branch
echo ""
echo "Do you want to push this backup branch to remote? (yes/no)"
```

After your backup is created, you'll be asked if you want to push it to the remote repository.
If you choose to push it, the command will run:

```bash
git push -u origin "$BACKUP_BRANCH"
```

This creates a complete, timestamped backup of your current branch state that you can return to at any time.

Note: To clean up your backup branches later, use `/project:git:cleanup-backups`