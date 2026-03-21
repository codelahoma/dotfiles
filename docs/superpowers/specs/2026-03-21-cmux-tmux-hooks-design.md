# cmux + tmux: Claude Code Hooks Integration

**Date:** 2026-03-21
**Branch:** feat/cmux-tmux-hooks
**Status:** Approved

## Problem

When running Claude Code inside a tmux session attached from a cmux pane, cmux receives no lifecycle signals. tmux sits between Claude and cmux's renderer, consuming OSC escape sequences. The `cmux notify` CLI (used by hooks) bypasses this via Unix socket IPC, but two issues remain:

1. `CMUX_WORKSPACE_ID` is not propagated into tmux — `cmux notify` can't target the correct workspace tab.
2. The `Notification` hook event (Claude blocked waiting for input) is not installed by default — the most important signal for the blue ring / attention indicator is missing.

## Solution

### 1. `ta()` function in `home/.zshrc`

Wraps `tmux attach` to propagate cmux env vars into tmux's global environment before attaching. No-ops safely when cmux is not present.

```bash
function ta() {
  if [ -n "$CMUX_WORKSPACE_ID" ]; then
    tmux set-environment -g CMUX_WORKSPACE_ID "$CMUX_WORKSPACE_ID"
    tmux set-environment -g CMUX_SOCKET_PATH "${CMUX_SOCKET_PATH:-/tmp/cmux.sock}"
  fi
  tmux attach "$@"
}
```

### 2. `~/.claude/hooks/cmux-notify.sh`

Hook script handling 4 events via `cmux notify` / `cmux set-status` over Unix socket (bypasses tmux terminal layer):

| Event | Action |
|-------|--------|
| `Stop` | Notify "Session complete" |
| `PostToolUse` (Task only) | Notify "Agent finished" |
| `Notification` | Notify with Claude's message + "Waiting" subtitle |
| `PreToolUse` | Set sidebar status pill showing current tool activity |

### 3. `~/.claude/settings.json` hooks section

Wire all 4 events to the hook script. `PostToolUse` uses matcher `Task`; others use empty matcher (match all).

## Files Changed

| File | Action | Repo-tracked |
|------|--------|-------------|
| `home/.zshrc` | Add `ta()` function | Yes |
| `~/.claude/hooks/cmux-notify.sh` | Create | No (user-local) |
| `~/.claude/settings.json` | Add hooks section | No (user-local) |

## Why the Socket CLI Approach Works Through tmux

`cmux notify` communicates via a Unix socket at `/tmp/cmux.sock`. This is pure IPC — it has nothing to do with the terminal hierarchy. A process running any number of tmux levels deep can call `cmux notify` and cmux will respond identically, as long as the socket is accessible and `CMUX_WORKSPACE_ID` is set.

## Testing

1. Start cmux, open a pane, run `tmux` inside it
2. Use `ta` to attach (or re-attach) — verify `echo $CMUX_WORKSPACE_ID` works inside tmux
3. Start Claude Code inside tmux
4. Verify notifications appear in cmux sidebar when Claude stops, agents finish, or Claude waits for input
5. Verify status pills update during tool use
