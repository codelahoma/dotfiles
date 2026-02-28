---
name: config-validator
description: Use when validating dotfiles configs, auditing for problems, checking for duplicate keybindings, orphaned noweb refs, stale generated files, or broken cross-references.
---

# Config Validator

## Overview

Validation checks to catch common dotfiles mistakes. Run after config changes or on demand.

## Source Files to Scan

| File | What to Check |
|---|---|
| `home/.hammerspoon/init.org` | hyper/magic/meh bindings |
| `home/dotspacemacs.org` | SPC-prefixed and custom Emacs bindings |
| `home/.zshrc` | shell keybindings (`bindkey`) |
| `home/.tmux.conf.local` | tmux bindings |
| `home/.config/karabiner/karabiner.json` | hardware remappings |

## Validation Checks

### 1. Duplicate Keybindings

Search all 5 sources for the same modifier+key combo bound in multiple systems.

```bash
# Hammerspoon bindings
grep -n 'hotkey.bind' home/.hammerspoon/init.org
# Tmux bindings
grep -n 'bind-key\|bind ' home/.tmux.conf.local
# Zsh bindings
grep -n 'bindkey' home/.zshrc
```

### 2. Orphaned Noweb References

Check that every `:noweb-ref` name used in assembly has at least one source block defining it, and vice versa.

```bash
# Find all noweb-ref declarations
grep -n ':noweb-ref' home/dotspacemacs.org
# Find all noweb references in tangle targets
grep -n '<<.*>>' home/dotspacemacs.org
```

### 3. Stale Generated Files

Generated files should never be newer than their source. If they are, someone edited the generated file directly.

Compare timestamps:
- `home/dotspacemacs.org` vs `home/.spacemacs.d/init.el`
- `home/.hammerspoon/init.org` vs `home/.hammerspoon/init.lua`

### 4. Untangled Changes

Source `.org` modified more recently than generated output means changes haven't been tangled yet.

### 5. Broken Cross-References

Hammerspoon config calling `emacsclient -e '(function-name ...)'` — verify the function exists in Spacemacs config.

## Output Format

Report each issue as:

```
[ERROR|WARNING] file:line — description
```

- **ERROR**: Will cause runtime failure (broken ref, orphaned noweb)
- **WARNING**: Potential issue (stale file, possible duplicate binding)
