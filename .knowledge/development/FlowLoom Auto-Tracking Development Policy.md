---
title: FlowLoom Auto-Tracking Development Policy
type: note
permalink: development/flow-loom-auto-tracking-development-policy
---

# FlowLoom Auto-Tracking Development Policy

## Context
This policy applies to ALL FlowLoom development work until pre-release cleanup.

## Required Auto-Tracking
All Claude Code instances working on FlowLoom must use the auto-tracking system to create detailed interaction logs.

## Implementation
1. **Get Shell PID**: Use `./bin/get_shell_pid.sh` to get current shell PID
2. **Track Every Interaction**: After each significant interaction, add observation to "FlowLoom Development Activity Tracking" entity
3. **Use Standard Format**: 
   ```
   Shell_ID: [PID] - AUTO: [timestamp] | Request: [user_request] | Tools: [tools_used] | Files: [files_affected] | Decisions: [decisions_made] | Next: [next_steps]
   ```

## Purpose
- Create comprehensive development audit trail
- Track all interactions, decisions, and changes during initial development
- Support analysis and development workflow optimization
- Will be removed during pre-release cleanup

## Auto-Track Command Location
`.claude/commands/flowloom/system/auto-track.md` contains the full command specification.

## Enforcement
This policy is active on ALL branches during initial development. Do not skip auto-tracking for any significant development interaction.

## Timeline
- **Active**: Now through pre-release cleanup
- **Removal**: During pre-release phase when FlowLoom is prepared for final product release