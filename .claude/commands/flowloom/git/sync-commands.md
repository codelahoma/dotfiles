Let input_args = "$ARGUMENTS"

I'll sync your Claude commands from the project's .claude directory to ${FLOWLOOM_WORK_DIR:-.meta-claude}/backup/.claude, back up configuration files, commit the changes, and push to the remote repository using the enhanced sync script.

## Running Enhanced Sync Script
The sync script now handles all operations including file backup, sync, and git operations.

@bash
if [ -z "$input_args" ]; then
  ./bin/sync_claude_commands.sh --verbose --commit --push
else
  ./bin/sync_claude_commands.sh --verbose --commit --push "$input_args"
fi

## Sync Complete

Show the user the output from the sync script above, including:
- Files that were synchronized
- Git commit details  
- Push status
- Any errors or warnings

