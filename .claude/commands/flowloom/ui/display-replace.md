# Replace Section in Second Screen Display

## Purpose
Replace a specific section in the active second screen display file.

## Arguments
Expected format: `section_header|new_content`
- First part: Section header to find and replace
- Second part: New content for that section

## Implementation

@bash
# Parse arguments
if [[ "$ARGUMENTS" != *"|"* ]]; then
    echo "❌ Invalid format. Use: section_header|new_content"
    exit 1
fi

SECTION_HEADER="${ARGUMENTS%%|*}"
NEW_CONTENT="${ARGUMENTS#*|}"

./.flowloom/bin/display_replace.py "$SECTION_HEADER" "$NEW_CONTENT"