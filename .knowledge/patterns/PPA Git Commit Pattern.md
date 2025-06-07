---
title: PPA Git Commit Pattern
type: note
permalink: patterns/ppa-git-commit-pattern
tags:
- '#ppa'
- '#git'
- '#permissions'
- '#patterns'
---

# PPA Git Commit Pattern

## Overview

The Principle of Permissionless Architecture (PPA) approach to git commits avoids requiring `rm` permissions by using file truncation instead of deletion.

## Implementation

```bash
# Create temp directory structure
mkdir -p .flowloom/.tmp

# Write commit message to file
echo "feat: add new feature" > .flowloom/.tmp/commit-msg.txt

# Commit using the file
git commit -F .flowloom/.tmp/commit-msg.txt

# Truncate instead of delete (no rm permission needed!)
echo -n > .flowloom/.tmp/commit-msg.txt
```

## Benefits

1. **No `rm` permission required** - Works within standard FlowLoom permissions
2. **Cleaner than `-m`** - Supports multi-line messages naturally
3. **Reusable location** - Same file path every time
4. **No cleanup issues** - Truncated file ready for next use
5. **Git-ignored** - `.tmp/` directory not tracked

## File Structure

```
.flowloom/
├── .tmp/
│   └── commit-msg.txt    # Temporary commit messages
├── sessions/
└── ...
```

## Usage in FlowLoom Commands

All FlowLoom commands that create commits should:
1. Generate message content
2. Write to `.flowloom/.tmp/commit-msg.txt`
3. Use `git commit -F`
4. Truncate the file

## Important Notes

- Ensure `.flowloom/.tmp/` is in `.gitignore`
- The truncation command `echo -n >` works cross-platform
- Empty file won't interfere with future commits
- Can also use `> file` or `: > file` for truncation

This pattern exemplifies PPA: achieving the goal (clean commit workflow) without requesting additional permissions.