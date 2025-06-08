# Slashload Startup Experiment

## What We Did
Added `slashload startup/context` to CLAUDE.local.md to automatically load startup context on initialization.

## Expected Behavior
- Startup context should load automatically when FlowLoom AI initializes
- Should provide immediate access to methodology, current state, and command patterns
- Should prevent need for manual context recovery

## Test Approach
Rod will test this by starting a new session and checking if context is available immediately.

## Trigger Pattern
If first prompt is "stat", FlowLoom AI should:
1. Confirm general knowledge of methodologies and tools
2. Summarize what we've been working on
3. Show understanding of current context and next steps

This tests whether the slashload successfully provides startup context.