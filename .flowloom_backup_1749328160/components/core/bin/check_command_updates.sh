#!/bin/bash
# check_command_updates.sh
# Script to check for updated Claude command files and record their timestamps
# For use with MCP memory tools to track command file changes

# Get the current timestamp
CURRENT_TIMESTAMP=$(date +"%Y-%m-%d %H:%M:%S")
CURRENT_EPOCH=$(date +%s)

echo "Command Update Checker"
echo "======================"
echo "Current timestamp: $CURRENT_TIMESTAMP"
echo

# Find all command markdown files and their modification times
echo "Checking for command file updates..."
find .claude/commands -name "*.md" -type f -not -path "*/\.*" | while read -r file; do
    # Get file modification time in epoch seconds
    MOD_TIME=$(stat -f "%m" "$file")
    # Get relative path for command ID
    REL_PATH=${file#.claude/commands/}
    
    # For comparing with memory system stored timestamps
    echo "command:$REL_PATH|$MOD_TIME"
done

echo
echo "To use this information:"
echo "1. Get the last check timestamp from CommandTrackerMetadata"
echo "2. Compare modification timestamps with stored values"
echo "3. Process only files with newer timestamps"
echo "4. Update CommandTrackerMetadata with current timestamp: $CURRENT_TIMESTAMP"