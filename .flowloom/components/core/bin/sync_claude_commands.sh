#!/usr/bin/env bash

# sync_claude_commands.sh
# 
# This script performs a one-way sync from the project's .claude directory to backup/.claude,
# and backs up configuration files (CLAUDE.md, CLAUDE.local.md, MCP configurations).
# This is a local-only operation - git operations are handled separately.
#
# Usage: ./bin/sync_claude_commands.sh [options]
#
# Options:
#   -v, --verbose    Show more detailed output
#   -d, --dry-run    Show what would be copied without making changes
#   -h, --help       Show this help message

set -e

# Initialize variables
VERBOSE=0
DRY_RUN=0
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
SOURCE_DIR="$PROJECT_ROOT/.claude"
DEST_DIR="$PROJECT_ROOT/backup/.claude"
CONFIG_BACKUP_DIR="$PROJECT_ROOT/backup/config"
RSYNC_OPTS="-a --delete"

# Process command line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    -v|--verbose)
      VERBOSE=1
      shift
      ;;
    -d|--dry-run)
      DRY_RUN=1
      RSYNC_OPTS="$RSYNC_OPTS -n"
      shift
      ;;
    -h|--help)
      echo "Usage: $(basename "$0") [options]"
      echo ""
      echo "Synchronizes the project's .claude directory to backup/.claude"
      echo "and backs up configuration files (CLAUDE.md, CLAUDE.local.md, MCP configurations)."
      echo "This is a local-only operation - git operations are handled separately."
      echo ""
      echo "Options:"
      echo "  -v, --verbose    Show more detailed output"
      echo "  -d, --dry-run    Show what would be copied without making changes"
      echo "  -h, --help       Show this help message"
      exit 0
      ;;
    -*)
      echo "Unknown option: $1"
      exit 1
      ;;
    *)
      echo "Unknown argument: $1"
      exit 1
      ;;
  esac
done

# Print header
echo "Claude Commands Backup Sync"
echo "==========================="
echo "Syncing from: $SOURCE_DIR"
echo "Syncing to:   $DEST_DIR"
echo ""

# Check if source directory exists
if [ ! -d "$SOURCE_DIR" ]; then
  echo "Error: Source directory '$SOURCE_DIR' does not exist."
  exit 1
fi

# Backup configuration files
echo "Backing up configuration files..."
if [ $DRY_RUN -eq 1 ]; then
  echo "[DRY RUN] Would create directory: $CONFIG_BACKUP_DIR"
  echo "[DRY RUN] Would copy CLAUDE.md, CLAUDE.local.md, and MCP configurations"
else
  mkdir -p "$CONFIG_BACKUP_DIR"
  if [ -f "$PROJECT_ROOT/CLAUDE.md" ]; then
    cp "$PROJECT_ROOT/CLAUDE.md" "$CONFIG_BACKUP_DIR/"
    echo "Backed up CLAUDE.md"
  fi
  if [ -f "$PROJECT_ROOT/CLAUDE.local.md" ]; then
    cp "$PROJECT_ROOT/CLAUDE.local.md" "$CONFIG_BACKUP_DIR/"
    echo "Backed up CLAUDE.local.md"
  fi
  if [ -f "$PROJECT_ROOT/.mcp.json" ]; then
    cp "$PROJECT_ROOT/.mcp.json" "$CONFIG_BACKUP_DIR/"
    echo "Backed up .mcp.json"
  fi
  if [ -f "$PROJECT_ROOT/.claude/settings.json" ]; then
    cp "$PROJECT_ROOT/.claude/settings.json" "$CONFIG_BACKUP_DIR/"
    echo "Backed up .claude/settings.json"
  fi
  if [ -f "$PROJECT_ROOT/.claude/settings.local.json" ]; then
    cp "$PROJECT_ROOT/.claude/settings.local.json" "$CONFIG_BACKUP_DIR/"
    echo "Backed up .claude/settings.local.json"
  fi
fi
echo ""

# Create destination directory if it doesn't exist
if [ ! -d "$DEST_DIR" ]; then
  if [ $DRY_RUN -eq 1 ]; then
    echo "[DRY RUN] Would create directory: $DEST_DIR"
  else
    echo "Creating destination directory: $DEST_DIR"
    mkdir -p "$DEST_DIR"
  fi
fi

# Add verbose flag if requested
if [ $VERBOSE -eq 1 ]; then
  RSYNC_OPTS="$RSYNC_OPTS -v"
fi

# Always add itemize-changes for better file tracking
RSYNC_OPTS="$RSYNC_OPTS --itemize-changes"

# Perform the sync
if [ $DRY_RUN -eq 1 ]; then
  echo "[DRY RUN] Showing what would be synced..."
else
  echo "Syncing files..."
fi

# Run rsync and capture output
RSYNC_OUTPUT=$(rsync $RSYNC_OPTS "$SOURCE_DIR/" "$DEST_DIR/" 2>&1)

# Filter rsync output to get actual file changes (ignore stats and non-file lines)
FILE_CHANGES=$(echo "$RSYNC_OUTPUT" | grep -E '^[><ch*]' 2>/dev/null || true)
if [ -n "$FILE_CHANGES" ] && [ "$FILE_CHANGES" != "" ]; then
  CHANGE_COUNT=$(echo "$FILE_CHANGES" | wc -l | tr -d ' ')
else
  CHANGE_COUNT=0
fi

# Report results
if [ $DRY_RUN -eq 1 ]; then
  if [ "$CHANGE_COUNT" -eq 0 ]; then
    echo "[DRY RUN] No changes would be made."
  else
    echo "[DRY RUN] $CHANGE_COUNT files would be updated:"
    echo "$FILE_CHANGES" | while IFS= read -r line; do
      if [ -n "$line" ]; then
        # Parse rsync itemize-changes format for dry-run
        if [[ $line =~ ^\>f\+\+\+\+\+\+\+\+\+ ]]; then
          file=$(echo "$line" | cut -c11-)
          echo "  WOULD ADD: $file"
        elif [[ $line =~ ^\>f\..*\.\.\.\.\. ]]; then
          file=$(echo "$line" | cut -c11-)
          echo "  WOULD UPDATE: $file"
        elif [[ $line =~ ^\*deleting ]]; then
          file=$(echo "$line" | cut -c11-)
          echo "  WOULD DELETE: $file"
        else
          file=$(echo "$line" | cut -c11-)
          echo "  WOULD CHANGE: $file"
        fi
      fi
    done
  fi
else
  if [ "$CHANGE_COUNT" -eq 0 ]; then
    echo "No changes made. Backup already up to date."
  else
    echo "Successfully synchronized $CHANGE_COUNT files:"
    echo "$FILE_CHANGES" | while IFS= read -r line; do
      if [ -n "$line" ]; then
        # Parse rsync itemize-changes format
        if [[ $line =~ ^\>f\+\+\+\+\+\+\+\+\+ ]]; then
          file=$(echo "$line" | cut -c11-)
          echo "  ADDED: $file"
        elif [[ $line =~ ^\>f\..*\.\.\.\.\. ]]; then
          file=$(echo "$line" | cut -c11-)
          echo "  UPDATED: $file"
        elif [[ $line =~ ^\*deleting ]]; then
          file=$(echo "$line" | cut -c11-)
          echo "  DELETED: $file"
        else
          file=$(echo "$line" | cut -c11-)
          echo "  CHANGED: $file"
        fi
      fi
    done
    echo ""
    echo "You can now commit the changes in backup/.claude to back them up."
  fi
fi

echo ""
echo "Done!"