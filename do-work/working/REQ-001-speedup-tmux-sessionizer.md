---
id: REQ-001
title: Speed up tmux-sessionizer startup
status: claimed
created_at: 2026-02-10T00:00:00Z
claimed_at: 2026-02-10T00:00:00Z
route: C
user_request: UR-001
---

# Speed up tmux-sessionizer startup

## What
Investigate and implement performance improvements for `home/bin/tmux-sessionizer`, potentially rewriting hot paths in a compiled language (Rust, Go, C).

## Context
The current script is ~489 lines of zsh. Key bottlenecks likely include:
- `build_directory_list_with_metadata()` runs `find` across multiple SEARCH_PATHS, then loops through every result calling `detect_project_type` (stat calls per dir) and `get_git_status` (git commands per dir)
- `format_directory_entry()` loops again through all results doing string formatting with `cut`/`printf`
- Stats tracking uses `jq` for JSON parsing on every directory entry
- `calculate_frecency_score` shells out to `bc` per entry

User suggested native code as an approach. The script currently has ENABLE_GIT_STATUS=false (noted as "SLOW") but other features like stats/frecency still add per-directory overhead.

---
*Source: can we do anything to speed up the startup of tmux-sessionizer? Maybe with native code?*

---

## Triage

**Route: C** - Complex

**Reasoning:** Performance optimization with multiple valid approaches (shell optimization vs partial native rewrite vs full native rewrite). Requires profiling, architectural decisions about language choice, and understanding the full script structure.

**Planning:** Required
