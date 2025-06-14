# FlowLoom Project Configuration

This file enables FlowLoom features and commands for this project.


## Slashload Command Definition

When I use 'slashload [filename] [arguments]', read the file `.claude/commands/[filename].md` as a prompt, replacing any `$ARGUMENTS` placeholders with the provided arguments.

## FlowLoom Configuration

slashload flowloom/config/development

## Startup Context Loading

slashload startup/context

## Special Trigger: "stat" on First Prompt

If the user's first prompt in a session is exactly "stat", provide an immediate status summary:

1. **Confirm Methodology Knowledge**: Show understanding of our dual memory systems (fl-memory.json + basic-memory), command patterns, and documentation workflow
2. **Current Context Summary**: What we've been working on, active objectives, recent discoveries
3. **Tool Proficiency**: Demonstrate knowledge of relative paths, --from-file patterns, and essential commands
4. **Next Steps**: Show awareness of pending todos and logical next actions

This tests whether the slashload startup context was successfully loaded and integrated.

## FlowLoom Installation Information
- **Installation Date**: 2025-06-07 15:26:45
- **FlowLoom Version**: 2.0.0
- **Components**: core, command-system, testing-framework, session-management, memory-system
- **Configuration Directory**: /Users/rodk/.homesick/repos/dotfiles/.flowloom
- **Bin and Src Directories**: Located under .flowloom


## Knowledge Base Management
- When adding to the knowledge base, create a document and use sync to add it.

## FlowLoom Commands Location
- All flowloom commands can be found under .claude/commands/flowloom