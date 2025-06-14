Enhanced FlowLoom footer command to include automatic memory logging prompts and dual system tracking reminders.

## Changes Made:
1. Added Memory Logging Protocol section to footer command
2. Included 4-step implementation with memory check reminder
3. Integrated memory status display in footer output
4. Designed to prompt AI to log activities before showing footer

## Purpose:
- Ensure consistent memory tracking across sessions
- Prevent loss of context and decisions
- Maintain dual memory system (fl-memory.json + basic-memory)
- Create automatic habit of logging via footer triggers

## Technical Approach:
- Modified .claude/commands/flowloom/system/footer.md
- Added explicit memory check step before footer display
- Integrated memory status into footer output
- Maintained interactive pause point functionality

This implements the user's vision of using footer as both information display and memory logging prompt mechanism.
