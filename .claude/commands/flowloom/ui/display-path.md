# Show Display Path

## Purpose
Show the path to the current active display file.

## Implementation

@bash
# Get the proper shell PID for finding our display file
if [[ -f "./.flowloom/bin/get_shell_pid.py" ]]; then
    SHELL_PID=$(./.flowloom/bin/get_shell_pid.py)
else
    SHELL_PID=$$  # Fallback to current process if script not found
fi
DISPLAY_FILE="claude_display_${SHELL_PID}.md"
DISPLAY_PATH="$(pwd)/.flowloom/displays/${DISPLAY_FILE}"

if [ -f "${DISPLAY_PATH}" ]; then
    echo "📍 Active display: ${DISPLAY_PATH}"
    echo "🆔 Session PID: ${SHELL_PID}"
    echo "📄 File: ${DISPLAY_FILE}"
else
    echo "❌ No active display found for PID ${SHELL_PID}"
    echo "💡 Create one with: slashload flowloom/ui/second-screen"
fi