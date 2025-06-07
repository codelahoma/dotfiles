---
title: 'Claude Code Permission Prompt: git commit to Main Branch'
type: note
permalink: troubleshooting/claude-code-permission-prompt-git-commit-to-main-branch
---

# Claude Code Permission Prompt: git commit to Main Branch

## Incident Details
- **Date:** 2025-01-27
- **Session:** session-5189-website  
- **Working Directory:** `/Users/rodk/github/flowloom/sessions/session-5189-website`

## Command Requiring Permission
```bash
git -C /Users/rodk/github/flowloom commit -m "chore: Add git stash to recent commands (synced from gh-pages)

Merged recent command from gh-pages branch while preserving main branch's unique commands.

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

## Permission Type
User confirmation prompt for git commit operation affecting main repository from a session worktree.

## Use Case Context
Syncing `.claude/settings.local.json` changes from gh-pages branch back to main branch. Successfully merged the `"Bash(git stash:*)"` recent command while preserving main branch's unique recent commands (`./bin/format-footer.sh`, `./bin/get_shell_pid.sh`).

## Permission Pattern
Claude Code prompted for user confirmation when:
- Using `git -C` to operate on main repository from session directory
- Making commits that affect the main development branch
- Multi-line commit messages with special formatting

## User Options Presented
1. **Yes** - Proceed with the commit
2. **No, and tell Claude what to do differently (esc)** - Cancel and provide alternative instructions

## Context
This is a **confirmation prompt**, not a security block. Claude Code asks for explicit user permission when making significant changes to the main repository from a session context.

## Resolution
User should select "Yes" to proceed with the commit, as this is the intended sync operation from gh-pages to main.

## Pattern Recognition
- Git operations affecting main repository trigger confirmation prompts
- Cross-branch file synchronization requires explicit user approval
- Session-to-main operations are treated as significant changes requiring confirmation

## Best Practices
- Always provide clear commit messages explaining the sync operation
- Document the source branch and merge strategy in commit message
- Use `git -C` for cross-directory repository operations from sessions