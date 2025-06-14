# Display History Management

## Purpose
List display history with navigation chain information. The history is automatically managed by the display system.

## Arguments
- No arguments: List recent displays for current session

## Implementation

@bash
# Get the proper shell PID for finding our display files
if [[ -f "./.flowloom/bin/get_shell_pid.py" ]]; then
    SHELL_PID=$(./.flowloom/bin/get_shell_pid.py)
else
    SHELL_PID=$$  # Fallback to current process if script not found
fi

DISPLAYS_DIR="$(pwd)/.flowloom/displays"
HISTORY_DIR="${DISPLAYS_DIR}/history"

# Ensure directories exist
mkdir -p "${HISTORY_DIR}"

# List recent displays for this session
echo "📜 Display History for Session ${SHELL_PID}:"
echo ""

if [ -d "${HISTORY_DIR}" ] && [ "$(ls -A "${HISTORY_DIR}" 2>/dev/null)" ]; then
    # Find archives for this session and sort by creation time
    ls -lt "${HISTORY_DIR}"/archived_*_claude_display_${SHELL_PID}.md 2>/dev/null | head -10 | while read -r line; do
        filename=$(basename $(echo "$line" | awk '{print $NF}'))
        date_info=$(echo "$line" | awk '{print $6, $7, $8}')
        
        # Extract timestamp from filename for better display
        if [[ "$filename" =~ archived_([0-9]{8}_[0-9]{6})_ ]]; then
            timestamp="${BASH_REMATCH[1]}"
            formatted_time=$(echo "$timestamp" | sed 's/_/ /' | sed 's/\([0-9]\{4\}\)\([0-9]\{2\}\)\([0-9]\{2\}\) \([0-9]\{2\}\)\([0-9]\{2\}\)\([0-9]\{2\}\)/\1-\2-\3 \4:\5:\6/')
            echo "  📄 ${filename}"
            echo "      🕒 ${formatted_time}"
        else
            echo "  📄 ${filename} (${date_info})"
        fi
    done
else
    echo "  No display history found for this session"
fi

# Show current display info
CURRENT_DISPLAY="${DISPLAYS_DIR}/claude_display_${SHELL_PID}.md"
if [ -f "${CURRENT_DISPLAY}" ]; then
    echo ""
    echo "📍 Current Display: claude_display_${SHELL_PID}.md"
    echo "    Last modified: $(stat -f "%Sm" -t "%Y-%m-%d %H:%M:%S" "${CURRENT_DISPLAY}")"
else
    echo ""
    echo "❌ No current display found"
    echo "💡 Create one with: slashload flowloom/ui/second-screen"
fi

echo ""
echo "ℹ️  Navigation is automatic:"
echo "   • Use ◀▶ arrows in the display to navigate history"
echo "   • History chain is managed automatically"
echo "   • Welcome screens are not archived"