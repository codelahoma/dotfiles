---
title: Claude Code Permission Block - Git Commit with HEREDOC
type: note
permalink: troubleshooting/claude-code-permission-block-git-commit-with-heredoc
---

# Claude Code Permission Block: Git Commit with HEREDOC

**Date**: May 27, 2025  
**Session**: 11110-docs  
**Context**: Documentation updates including Atlas UP business case

## Command Attempted
```bash
git commit -m "$(cat <<'EOF'
feat: Add Atlas UP open source business case and update documentation
[... multi-line commit message ...]
EOF
)"
```

## Permission Issue
- **User Action**: Denied permission request from Claude Code
- **Permission Details**: Not captured (user noted to capture in future)
- **Trigger**: Likely the HEREDOC syntax or multi-line commit message format

## Context & Goal
- Committing documentation updates including:
  - New Atlas UP open source business case document
  - GitHub username corrections across docs
  - Quick-start guide installer step addition
  - Project facts tracking enhancement
- Working in isolated git worktree session

## Alternative Solutions to Try
1. **Simple commit message**: Use basic single-line commit without HEREDOC
2. **Standard git commit**: Avoid shell scripting constructs
3. **Interactive commit**: Use `git commit` without `-m` flag
4. **File-based message**: Write message to file, use `git commit -F`

## Notes
- HEREDOC in git commands may trigger security warnings
- Multi-line commit messages might require different approach
- Session isolation should make git operations safe

## Next Steps
- Try simpler commit approach
- Capture exact permission details in future blocks
- Document successful workaround approach