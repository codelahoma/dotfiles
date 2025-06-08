Footer script enhancement - added automatic commit/push suggestion when git changes detected.

## Changes Made:
1. Added has_git_changes() function to detect modified/untracked files
2. Auto-generates next steps when default steps used and changes present
3. Inserts 'Commit and push changes' as first option when changes detected

## Purpose:
- Make footer more actionable for git workflow
- Maintain clean git hygiene automatically
- Provide relevant next steps based on current repository state

This enhancement makes the footer context-aware and helps with development workflow automation.
