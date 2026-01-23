# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a dotfiles repository using **Homeshick** conventions. All dotfiles live under `home/` and get symlinked to `~` by homeshick. The repository uses **literate configuration** extensively—`.org` files tangle to generate actual config files.

**Important:** Files in this repo are symlinked to `~` maintaining the same hierarchy. There's never a need to "check from the home directory" - always work with files in this repo directly. If symlinks seem missing, run `homeshick link -f dotfiles` to ensure all links are in place.

## Key Commands

### Tangling Literate Configs

After editing any `.org` config file, tangle it to regenerate the output:

```bash
# Tangle Spacemacs config
emacs --batch -l org --eval '(org-babel-tangle-file "home/dotspacemacs.org")'

# Tangle Hammerspoon config
emacs --batch -l org --eval '(org-babel-tangle-file "home/.hammerspoon/init.org")'
```

Or from within Emacs: `C-c C-v t` on the org file.

### Homeshick Operations

```bash
# Link dotfiles to home directory
homeshick link dotfiles

# Check for unlinked files
homeshick check dotfiles
```

### Email (mu4e)

```bash
# Sync mail
mbsync -a

# Reindex after config changes (all addresses are aliases via Fastmail)
mu init --maildir=~/Maildir/Fastmail \
  --my-address=rod@rodknowlton.com \
  --my-address=rod.knowlton@gmail.com \
  --my-address=codelahoma@gmail.com \
  --my-address=knowshank@knowshank.com \
  --my-address=knowshank@fastmail.com
mu index
```

## Architecture

### Literate Config Files (Source of Truth)

| Source File | Generates | Purpose |
|-------------|-----------|---------|
| `home/dotspacemacs.org` | `home/.spacemacs.d/init.el` | Spacemacs configuration |
| `home/.hammerspoon/init.org` | `home/.hammerspoon/init.lua` | macOS automation |
| `home/.hammerspoon/menuHammerCustomConfig.org` | `menuHammerCustomConfig.lua` | Modal menu system |

**Never edit generated files directly**—they contain "DO NOT EDIT" warnings and will be overwritten.

### Private Spacemacs Layers

Located in `home/.emacs.d/private/`:

- **`gtd-zettelkasten/`** — Symlink to external repo (`~/github/org-gtd-zettelkasten/layers/gtd-zettelkasten`). Full GTD workflow with org-roam integration. Keybindings under `SPC o o`.
- **`rk-layout/`** — Frame geometry persistence for GUI Emacs.

### Key Integrations

- **Emacs ↔ Hammerspoon**: URLs from Emacs route through Hammerspoon's URLDispatcher to select browser. Edit-in-Emacs support via editWithEmacs.spoon.
- **Email**: mu4e → mbsync → Fastmail. Passwords via `pass` (password-store) with auth-source-pass.
- **Shell**: Zsh with oh-my-zsh, powerlevel10k theme. `Ctrl-F` triggers tmux-sessionizer.

### Important Paths

- **Org files**: `~/Dropbox/org/` (GTD in `gtd/`, notes in `notes/`, reviews in `reviews/`)
- **Mail**: `~/Maildir/Fastmail/`
- **Password store**: `~/.password-store/`

### Git Submodules

- `home/.oh-my-zsh` → codelahoma/oh-my-zsh (rodk branch)

## Configuration Patterns

### Spacemacs Layer Variables

Layer-specific variables are set in the layer declaration in `dotspacemacs.org`, not in user-config:

```elisp
(mu4e :variables
      mu4e-installation-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
      mu4e-mu-binary "/opt/homebrew/bin/mu")
```

### Noweb References

The `dotspacemacs.org` uses noweb syntax. Source blocks tagged with `:noweb-ref config-layers` or `:noweb-ref user-config` are collected into the appropriate sections during tangling.

## Troubleshooting

### tmux Scrollback Issues

The tmux config has `allow-passthrough on` (line 446 of `.tmux.conf.local`) which lets applications send escape sequences directly to iTerm2. If scrollback stops working inside tmux:

1. **Quick fix**: Start a fresh tmux server (`tmux kill-server && tmux`)
2. **Diagnose**: Corrupted terminal state is often the cause, not config changes
3. **Test passthrough**: `tmux set -g allow-passthrough off` temporarily

To capture terminal escape sequences for debugging:

```bash
script -q /tmp/output.txt
# run commands, then exit
cat -v /tmp/output.txt | less  # look for ^[[2J, ^[[3J, ^[[?1049h
```

### Emacs Stuck / Can't Quit

If Emacs GUI is unresponsive or stuck in a prompt you can't escape:

```bash
# Graceful quit via emacsclient (saves buffers first)
emacsclient -e '(save-buffers-kill-emacs)'

# If that fails, send SIGTERM (gives Emacs chance to clean up)
pkill -TERM Emacs

# Last resort: force kill
pkill -9 Emacs
```

### mbsync Duplicate UID Errors

**Symptoms:** mu4e kicks you back to main menu after update, or shows "Update process returned with non-zero exit code"

**Diagnosis:**
```bash
mbsync -a 2>&1; echo "Exit code: $?"
```

Common errors:
- `Maildir error: duplicate UID X in /path/to/folder`
- `Maildir error: UID X is beyond highest assigned UID Y`

**History:**
- 2026-01-21: Trash folder had ~65 duplicate UIDs (111-299 range) plus UIDs beyond valid range (191840+). Old files from Oct 2024 sync conflicted with Jan 2026 files.
- 2026-01-21 (same day, later): Trashing a single email triggered duplicate UID 300. Root cause: 296 orphaned files from Oct 2024 (filename prefix `1768500288.*`) were never cleaned up and had UIDs overlapping with current UID assignments. **Fix: Remove ALL orphaned files from old sync batch, not just duplicates found so far.**

**Root cause pattern:** Old sync creates files, sync state gets corrupted/reset, server reassigns same UIDs to new emails, old orphaned files conflict. Check for files with old timestamps that don't match recent sync patterns.

**Fix duplicate UIDs:**
```bash
# Find duplicates in a folder
ls -1 ~/Maildir/Fastmail/Trash/cur/ | sed 's/.*,U=\([0-9]*\).*/\1/' | sort -n | uniq -d

# Find files for a specific UID
find ~/Maildir/Fastmail/Trash -name "*,U=204:*"

# Remove older duplicate (check timestamps, keep newer)
rm -f ~/Maildir/Fastmail/Trash/cur/OLDER_FILE_HERE
```

**Fix out-of-range UIDs:**
```bash
# Find UIDs beyond limit (check .uidvalidity for max)
ls ~/Maildir/Fastmail/Trash/cur/ | grep -o 'U=[0-9]*' | sed 's/U=//' | awk '$1 > 3073 {print}'

# Remove files with invalid UIDs - they'll re-sync from server
rm -f ~/Maildir/Fastmail/Trash/cur/*U=INVALID_UID*
```

After fixing, run `mbsync -a` again until exit code is 0, then reindex mu4e:
```bash
mu index --rebuild
```
