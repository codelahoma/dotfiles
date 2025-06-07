---
title: Manual Session Creation Process
type: note
permalink: session-management/manual-session-creation-process
---

# Manual Session Creation Process

This document provides the step-by-step manual process for creating FlowLoom sessions without using the session_manager.sh script. This is useful when the automated scripts have issues or for understanding the underlying session creation mechanics.

## Manual Session Creation Steps

### 1. Set Session Parameters
```bash
PURPOSE="your-session-purpose"
SHELL_PID=$(ps -o ppid= -p $$)
SESSION_ID="${SHELL_PID}-${PURPOSE}"
WORKTREE_NAME="shell-instance-${SHELL_PID}"
```

### 2. Create Git Branch and Worktree
```bash
git worktree add -b "session/${SESSION_ID}" "sessions/${WORKTREE_NAME}"
```

### 3. Create Session Directory Structure
```bash
mkdir -p "sessions/${WORKTREE_NAME}/sessions/${SESSION_ID}/artifacts"
```

### 4. Change to Session Directory
```bash
cd "sessions/${WORKTREE_NAME}"
```

### 5. Create Session Metadata
```bash
python3 -c "
import json
from datetime import datetime
metadata = {
    'sessionId': '${SESSION_ID}',
    'shellPid': ${SHELL_PID},
    'purpose': '${PURPOSE}',
    'createdManually': True,
    'startTime': datetime.now().isoformat(),
    'workingDirectory': '$(pwd)',
    'gitBranch': 'session/${SESSION_ID}',
    'worktreeName': '${WORKTREE_NAME}',
    'status': 'active'
}
with open('sessions/${SESSION_ID}/metadata.json', 'w') as f:
    json.dump(metadata, f, indent=2)
"
```

### 6. Verify Session Setup
```bash
# Check you're in the session worktree
pwd
# Verify metadata was created
cat sessions/${SESSION_ID}/metadata.json
```

## Additional Operations

### List Available Sessions
```bash
ls -d sessions/shell-instance-*/ 2>/dev/null
```

### Switch to Different Session
```bash
cd sessions/${TARGET_WORKTREE_NAME}
```

### Clean Up Session When Done
```bash
# From main directory
git worktree remove "sessions/${WORKTREE_NAME}"
git branch -d "session/${SESSION_ID}"
```

## Key Design Decisions

- **Shell PID for Uniqueness**: Uses shell process ID to ensure unique session identification
- **Purpose-based Naming**: Includes user-defined purpose in session ID for clarity
- **Worktree Isolation**: Each session gets its own Git worktree for complete file isolation
- **Structured Metadata**: JSON metadata files provide programmatic session management
- **shell-instance Prefix**: Avoids naming conflicts with sessions/ subdirectory

## When to Use Manual Process

- Automated session scripts are failing
- Debugging session creation issues
- Understanding session architecture
- Testing session isolation capabilities
- Emergency session recovery scenarios

## Architecture Notes

The manual process follows the same architectural principles as the automated system:
- Git worktree-based isolation
- Shell PID tracking for session correlation
- Structured metadata for session management
- Predictable directory layout for tooling integration