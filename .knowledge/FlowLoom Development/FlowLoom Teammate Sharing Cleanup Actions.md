---
title: FlowLoom Teammate Sharing Cleanup Actions
type: note
permalink: flow-loom-development/flow-loom-teammate-sharing-cleanup-actions
tags:
- '#cleanup'
- '#sharing'
- '#teammates'
---

# FlowLoom Teammate Sharing Cleanup Actions

## Context
Preparing FlowLoom repository for sharing with Atlas UP teammates. Most content can remain since these are internal teammates.

## Required Actions

### 1. Remove Rabbit Story
```bash
rm "/Users/rodk/github/flowloom/.knowledge/FlowLoom Development/The Tale of Recursive Rabbit - A FlowLoom Bedtime Story.md"
```

### 2. Fix Sessions Directory Gitignore
The `/sessions/` directory is NOT currently in .gitignore (only `.flowloom/sessions/` is).

Add to .gitignore:
```
# Session directories
/sessions/
```

### 3. Remove Sessions from Git Tracking
```bash
git rm -r --cached sessions/
git commit -m "Remove session directories from tracking"
```

## What Can Stay
- Personal information (Rod Knowlton, emails) - OK for teammates
- File paths with /Users/rodk/ - OK for teammates  
- All Atlas UP references - these ARE the teammates
- CLAUDE.local.md and settings.local.json - FlowLoom product defaults
- TODO/FIXME comments - all benign

## Summary
Minimal cleanup needed:
1. Delete one whimsical rabbit story
2. Properly gitignore sessions directory
3. Remove already-committed sessions from tracking