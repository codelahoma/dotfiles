---
title: Git Operations Best Practices
type: note
permalink: setup/git-operations-best-practices
tags:
- '#git'
- '#version-control'
- '#best-practices'
- '#mcp'
---

# Git Operations Best Practices

## Critical Warning: Avoid MCP Git Server

**⚠️ NEVER USE MCP GIT SERVER**: The MCP git server (`mcp__git__*`) has critical issues that can corrupt repositories. FlowLoom explicitly denies all MCP git permissions.

### Why This Matters
During FlowLoom development, we discovered that the MCP git server could:
- Corrupt git object databases
- Create invalid commits
- Break repository integrity
- Cause unrecoverable data loss

### Safe Git Operations

Always use standard bash git commands:
```bash
git status
git add .
git commit -m "message"
git push
git pull
```

These are reliable, well-tested, and won't corrupt your repository.

## FlowLoom Git Workflow

### 1. Sync Command
FlowLoom provides a streamlined `sync` command that:
- Checks for uncommitted changes
- Auto-generates commit messages
- Pulls latest changes
- Pushes your commits

Just type: `sync`

### 2. Commit Message Generation
FlowLoom can analyze your changes and suggest meaningful commit messages:
- Reviews modified files
- Understands change context
- Follows conventional commit format
- Adds semantic prefixes (feat, fix, docs, etc.)

### 3. Safe Operations
All FlowLoom git operations use bash commands:
```json
"Bash(git status:*)",
"Bash(git add:*)",
"Bash(git commit:*)",
"Bash(git diff:*)",
"Bash(git log:*)"
```

## Best Practices

### 1. Atomic Commits
- One logical change per commit
- Clear, descriptive messages
- Reference issues when applicable

### 2. Branch Strategy
FlowLoom supports multiple workflows:
- **Feature branches**: `feature/description`
- **Session branches**: `session/timestamp-context`
- **Worktree isolation**: Separate worktrees for parallel work

### 3. Commit Early and Often
- Don't wait for "perfect" code
- Commit working increments
- Use `git stash` for temporary work

### 4. Review Before Pushing
Always review changes:
```bash
git status           # See what's changed
git diff            # Review modifications  
git log --oneline   # Check commit history
```

## FlowLoom-Specific Features

### Session-Based Commits
FlowLoom can track work sessions and create meaningful commits:
- Groups related changes
- Adds session context
- Maintains work history

### Worktree Management
For parallel development:
```bash
git worktree add -b session/new-feature ../feature-work
```

### Backup Integration
FlowLoom automatically:
- Creates pre-operation backups
- Stores them in `.flowloom_backup_*/`
- Provides recovery options

## Common Issues and Solutions

### Merge Conflicts
1. Pull before starting work
2. Communicate with team
3. Use FlowLoom's merge assistance

### Large Files
1. Use `.gitignore` properly
2. Consider git-lfs for binaries
3. Keep repositories focused

### Detached HEAD
1. Create a branch: `git checkout -b recovery`
2. Don't panic - commits are safe
3. Use FlowLoom's recovery tools

## Integration with Memory System

FlowLoom tracks git operations in memory:
- Commit summaries in daily notes
- Important decisions in knowledge base
- Session work in memory graph

## Emergency Recovery

If something goes wrong:
1. **Stop** - Don't make it worse
2. **Check backups** - `.flowloom_backup_*/`
3. **Use reflog** - `git reflog` shows recent operations
4. **Ask for help** - FlowLoom commands can assist

Remember: Git is forgiving. Almost everything can be recovered if you haven't force-pushed or garbage collected.