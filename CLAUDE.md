# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a dotfiles repository using **Homeshick** conventions. All dotfiles live under `home/` and get symlinked to `~` by homeshick. The repository uses **literate configuration** extensively—`.org` files tangle to generate actual config files.

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

# Reindex after config changes
mu init --maildir=~/Maildir/Fastmail --my-address=knowshank@fastmail.com
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
