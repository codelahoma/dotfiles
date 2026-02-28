---
name: literate-config
description: Use when editing .org config files, tangling, or working with literate configuration. Triggers on "edit org", "tangle", "literate config", "noweb", or any modification to dotspacemacs.org, init.org, or menuHammerCustomConfig.org.
---

# Literate Config Editing

## Overview

This dotfiles repo uses literate configuration: `.org` files are the source of truth, tangled to generate actual config files. Never edit generated files directly.

## Source-of-Truth Mappings

| Source (.org) | Generated Output | Domain |
|---|---|---|
| `home/dotspacemacs.org` | `home/.spacemacs.d/init.el` | Spacemacs |
| `home/.hammerspoon/init.org` | `home/.hammerspoon/init.lua` | Hammerspoon |
| `home/.hammerspoon/menuHammerCustomConfig.org` | `menuHammerCustomConfig.lua` | MenuHammer |

## Tangle Command

```bash
emacs --batch -l org --eval '(org-babel-tangle-file "FILE")'
```

## Noweb Syntax

Source blocks use `:noweb-ref` headers to collect into named sections during tangling:
- `:noweb-ref config-layers` — assembles into `dotspacemacs-configuration-layers`
- `:noweb-ref user-config` — assembles into `dotspacemacs/user-config`
- `:noweb-ref additional-packages` — assembles into `dotspacemacs-additional-packages`

Tangle control: `:tangle yes` (default for most) or `:tangle no` to exclude a block.

## Enforced Workflow

**You MUST follow all 5 steps. No shortcuts.**

1. **Read** the `.org` source file
2. **Edit** the `.org` file (NEVER the generated output)
3. **Tangle** using the emacs batch command above
4. **Verify** the generated file contains the expected change (grep or diff)
5. **Reload reminder**:
   - Hammerspoon: remind user to reload (`hs.reload()` or `hyper+r`)
   - Spacemacs: remind user to restart (`SPC q r`) or reload config (`SPC f e R`)

## Red Flags

- Editing `init.el`, `init.lua`, or any file with "DO NOT EDIT" at the top
- Skipping the tangle step after editing
- Skipping verification after tangling
