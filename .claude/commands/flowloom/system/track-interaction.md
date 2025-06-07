# Track Single Interaction

Let input_args = "$ARGUMENTS"

Manually track a specific development interaction with comprehensive details.

## Purpose

This is for one-off manual tracking when auto-tracking is disabled or when you want to capture a specific moment with custom context.

## Argument Interpretation

Process the tracking request: input_args

- Default behavior: Track the most recent interaction based on current context
- With arguments: Track with custom context or description

## Manual Tracking Process

1. **Detect Current Shell PID**
   Use stored shell PID: 83271 (obtained from ./bin/get_shell_pid.sh at session start)

2. **Track the Interaction**
   Add detailed observation to the "FlowLoom Development Activity Tracking" entity including:
   - Shell PID for session correlation
   - Interaction timestamp and context  
   - User request or command issued
   - Tools used in response
   - Files modified or created
   - Decisions made
   - Next steps identified

3. **Tracking Format**
   "Shell_ID: [PID] - MANUAL: [timestamp] | Request: [user_request] | Tools: [tools_used] | Files: [files_affected] | Decisions: [decisions_made] | Next: [next_steps]"

## Implementation

```bash
# Get current timestamp
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Get shell PID
SHELL_PID=83271

# Create manual tracking entry
echo "📝 MANUAL TRACK: Recording interaction for shell session $SHELL_PID"

# Add observation to tracking entity
# Include MANUAL flag to distinguish from auto-tracking
```

**Usage Examples**:
- `/project:system:track-interaction` - Track current context
- `/project:system:track-interaction "completed installer update"` - Track with custom description
- `/project:system:track-interaction "decision point: chose mobile app approach"` - Track specific decision

**Note**: This complements the auto-tracking toggle. Use this for important moments you want to ensure are captured, regardless of auto-tracking state.