#!/bin/bash
# cmux + tmux Claude Code notification bridge
# Works because cmux notify talks to /tmp/cmux.sock directly,
# bypassing the tmux terminal layer entirely.

[ -S "${CMUX_SOCKET_PATH:-/tmp/cmux.sock}" ] || exit 0

EVENT=$(cat)
EVENT_TYPE=$(echo "$EVENT" | jq -r '.hook_event_name // "unknown"')
TOOL=$(echo "$EVENT" | jq -r '.tool_name // ""')

# Pass --workspace so cmux rings the correct tab even when user is elsewhere
WS_ARGS=()
[ -n "$CMUX_WORKSPACE_ID" ] && WS_ARGS=(--workspace "$CMUX_WORKSPACE_ID")

# One-shot: rename workspace to bare repo name on first hook invocation
if [ -n "$CMUX_WORKSPACE_ID" ]; then
    INIT_MARKER="/tmp/cmux-init-${CMUX_WORKSPACE_ID}"
    if [ ! -f "$INIT_MARKER" ]; then
        touch "$INIT_MARKER"
        REPO_ROOT=$(git rev-parse --show-toplevel 2>/dev/null)
        if [ -n "$REPO_ROOT" ]; then
            cmux rename-workspace --workspace "$CMUX_WORKSPACE_ID" -- "$(basename "$REPO_ROOT")" 2>/dev/null
        fi
    fi
fi

case "$EVENT_TYPE" in
    "Stop")
        cmux notify --title "Claude Code" --body "Session complete" "${WS_ARGS[@]}"
        ;;
    "PostToolUse")
        [ "$TOOL" = "Task" ] && \
            cmux notify --title "Claude Code" --body "Agent finished" "${WS_ARGS[@]}"
        ;;
    "Notification")
        MSG=$(echo "$EVENT" | jq -r '.message // "Needs your input"')
        cmux notify --title "Claude Code" --subtitle "Waiting" --body "$MSG" "${WS_ARGS[@]}"
        ;;
    "PreToolUse")
        case "$TOOL" in
            "Bash")
                CMD=$(echo "$EVENT" | jq -r '.tool_input.command // ""' | cut -c1-60)
                cmux set-status claude "running: $CMD" --icon terminal --color "#ff9f0a" "${WS_ARGS[@]}"
                ;;
            "Edit"|"MultiEdit"|"Write")
                FILE=$(echo "$EVENT" | jq -r '.tool_input.file_path // ""' | xargs basename 2>/dev/null)
                cmux set-status claude "editing: $FILE" --icon pencil --color "#0a84ff" "${WS_ARGS[@]}"
                ;;
        esac
        ;;
esac
