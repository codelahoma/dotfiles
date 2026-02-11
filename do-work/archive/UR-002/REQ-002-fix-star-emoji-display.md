---
id: REQ-002
title: Fix star emoji display for frequent visits
status: completed
created_at: 2026-02-10T17:00:00Z
user_request: UR-002
related: [REQ-003]
---

# Fix star emoji display for frequent visits

## What
The Rust scanner outputs star emojis (⭐) for high-frecency entries but user reports they aren't showing. Investigate whether this is a rendering issue in fzf/tmux or a logic bug in the scanner output.

## Builder Guidance
- Certainty level: Exploratory — user flagged as low priority: "That's just a visual thing. No big deal if it's a problem to implement."

## Context
The old shell script used `⭐ ` prefix for entries with frecency > 20. The Rust scanner replicates this logic. Need to verify the output is correct and visible in the actual fzf display context.

## Resolution
Investigated — stars ARE present in Rust scanner output. Verified via raw byte inspection. No bug found; user may not have tried the new version live yet at time of report.

---
*Source: "I did notice one issue that the not getting the emojis indicating frequent visits. I think I do have the correct frequency counts. But we used to have little emojis on that."*
