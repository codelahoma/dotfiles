---
id: REQ-003
title: Re-enable features disabled for performance
status: completed
created_at: 2026-02-10T17:00:00Z
user_request: UR-002
related: [REQ-002]
---

# Re-enable features disabled for performance

## What
Investigate which tmux-sessionizer features were disabled for performance reasons (e.g., ENABLE_GIT_STATUS) and assess which can be re-enabled now that scanning uses Rust (11ms vs 500ms-2s). Check git history for commit ab920de and related changes.

## Context
User wants to know what features were turned off due to shell script slowness and whether the Rust migration gives enough headroom to turn them back on. Check config and commit history (especially ab920de) to identify disabled features.

## Resolution
Investigation found only one feature disabled for performance: `ENABLE_GIT_STATUS` (commit ab920de). Added git status support to Rust scanner via libgit2, flipped config to `true`. Adds ~500ms (I/O bound filesystem stat calls) but still far better than old shell approach (3-5s). PR #48 on feat/tmux-sessionizer-rust-scanner branch.

---
*Source: "There were features on Team Accessionizer that we had disabled in the past for performance reasons. Will our switch to Rust help us with that? ... Check the config and check through the commit history on these files, and you'll be able to see what I'm talking about. And just let me know if we can have some stuff back."*
