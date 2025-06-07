# Auto-Track Development Activity

Let input_args = "$ARGUMENTS"

Capture current development interaction immediately for WORM governance.

## Usage

- `slashload flowloom/system/auto-track` - Capture current interaction
- `slashload flowloom/system/auto-track enable` - Capture current interaction (alias)

## What Gets Captured

Immediately captures current interaction to fl-memory.json in the format:
"Shell_ID: [PID] - AUTO: [timestamp] | Request: [user_request] | Tools: [tools_used] | Files: [files_affected] | Decisions: [decisions_made] | Next: [next_steps]"

## Implementation

@bash
# Create immediate memory observation using the proper observation gatekeeper
./bin/log_observation.py add-observation "Session-$$-$(date +%s)-current" "Session" "Shell_ID: $$ - AUTO: $(date -u +%Y-%m-%dT%H:%M:%SZ) | Request: auto_track_enabled | Tools: [manual_capture] | Files: fl-memory.json | Decisions: user_initiated_tracking | Next: continue_development"

Show that auto-tracking observation has been captured for current session.

## Manual Capture Workflow

1. Run command when you want to capture current state
2. Immediate capture of interaction context to fl-memory.json
3. No background processes or continuous monitoring
4. Use at key decision points for WORM governance