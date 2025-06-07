# Session Cleanup Command

Clean up abandoned session worktrees and branches.

## Context

You need to clean up session isolation artifacts:
- Remove orphaned worktrees from terminated sessions
- Delete session branches that were left for review
- Clean up session directories with no active processes
- Maintain worktree directory hygiene

## Task

Identify and clean up abandoned session artifacts:

@bash
# Find all session worktrees
find "${FLOWLOOM_WORK_DIR:-$HOME/.flowloom/worktrees}" -maxdepth 1 -type d -name "session-*" 2>/dev/null | while read worktree_dir; do
    if [[ -n "$worktree_dir" ]]; then
        session_short_id=$(basename "$worktree_dir" | sed 's/session-//')
        echo "Found worktree: $worktree_dir (session: $session_short_id)"
        
        # Check if git recognizes this worktree
        if git worktree list | grep -q "$worktree_dir"; then
            echo "  ✓ Valid git worktree"
            
            # Check if corresponding session branch exists
            branch_name="session/$session_short_id"
            if git rev-parse --verify "$branch_name" >/dev/null 2>&1; then
                echo "  ✓ Session branch exists: $branch_name"
                
                # Check if branch has commits ahead of main
                commits_ahead=$(git rev-list --count "main..$branch_name" 2>/dev/null || echo "0")
                if [[ "$commits_ahead" -gt 0 ]]; then
                    echo "  ⚠ Branch has $commits_ahead commits - review needed"
                else
                    echo "  ✓ Branch has no new commits - safe to cleanup"
                fi
            else
                echo "  ⚠ Session branch missing: $branch_name"
            fi
        else
            echo "  ⚠ Orphaned directory (not a valid git worktree)"
        fi
        echo ""
    fi
done

Show the user the output above including all session worktrees found and their status.

## Cleanup Actions

Based on the scan results, you can perform these cleanup actions:

**Safe Cleanup (no commits):**
```bash
# Remove worktree and delete branch
git worktree remove /path/to/session-abc123 --force
git branch -d session/abc123
```

**Review Required (has commits):**
```bash
# Check commits in session branch
git log main..session/abc123 --oneline

# If commits should be kept:
git checkout main
git merge session/abc123

# If commits are not needed:
git branch -D session/abc123  # Force delete
git worktree remove /path/to/session-abc123 --force
```

**Orphaned Directories:**
```bash
# Remove directory that's not a valid worktree
rm -rf /path/to/orphaned-session-dir
```

## Automated Cleanup Script

For safe automated cleanup of sessions with no commits:

@bash
echo "Starting automated cleanup of empty session branches..."

# Find sessions with no commits ahead of main
git for-each-ref --format='%(refname:short)' refs/heads/session/ | while read branch; do
    commits_ahead=$(git rev-list --count "main..$branch" 2>/dev/null || echo "0")
    if [[ "$commits_ahead" -eq 0 ]]; then
        echo "Cleaning up empty session branch: $branch"
        
        # Extract session ID from branch name
        session_id=$(echo "$branch" | sed 's|session/||')
        
        # Find corresponding worktree
        worktree_path="${FLOWLOOM_WORK_DIR:-$HOME/.flowloom/worktrees}/session-$session_id"
        
        # Remove worktree if it exists
        if [[ -d "$worktree_path" ]] && git worktree list | grep -q "$worktree_path"; then
            echo "  Removing worktree: $worktree_path"
            git worktree remove "$worktree_path" --force
        fi
        
        # Delete the branch
        echo "  Deleting branch: $branch"
        git branch -d "$branch"
        
        echo "  ✓ Cleaned up session: $session_id"
    else
        echo "Preserving session branch with $commits_ahead commits: $branch"
    fi
done

echo "Automated cleanup completed."

Show the user the cleanup results.

## Manual Review Process

For sessions requiring review:

1. **List sessions with commits:**
   ```bash
   git for-each-ref --format='%(refname:short)' refs/heads/session/ | while read branch; do
       commits=$(git rev-list --count "main..$branch" 2>/dev/null || echo "0")
       if [[ "$commits" -gt 0 ]]; then
           echo "$branch: $commits commits"
       fi
   done
   ```

2. **Review specific session:**
   ```bash
   git log main..session/abc123 --oneline --stat
   ```

3. **Decide on action:**
   - Merge if work should be kept
   - Cherry-pick specific commits
   - Force delete if work is not needed

## Notes

- Always scan before automated cleanup
- Preserve branches with commits for review
- Orphaned directories can be safely removed
- Use `--force` flag for worktree removal when needed
- Consider backup before bulk cleanup operations