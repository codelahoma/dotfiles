---
name: spacemacs-config
description: Use when working with Spacemacs, Emacs, layers, dotspacemacs, packages, or keybindings in the Emacs configuration.
---

# Spacemacs Config

## Overview

Spacemacs configured via literate config in `home/dotspacemacs.org`. Source is `dotspacemacs.org`, never `init.el`.

**REQUIRED:** Use `literate-config` skill for all edits.

## Noweb Assembly

Source blocks tagged with `:noweb-ref` collect into sections during tangling:

| Noweb Ref | Assembles Into |
|---|---|
| `config-layers` | `dotspacemacs-configuration-layers` list |
| `user-config` | `dotspacemacs/user-config` function |
| `additional-packages` | `dotspacemacs-additional-packages` list |

## Conventions

### Layer Variables

Set in the layer declaration, NOT in user-config:

```elisp
(mu4e :variables
      mu4e-installation-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
      mu4e-mu-binary "/opt/homebrew/bin/mu")
```

### Additional Packages

Add via a block with `:noweb-ref additional-packages`:

```org
#+begin_src emacs-lisp :noweb-ref additional-packages
package-name
#+end_src
```

### Keybinding Convention

User bindings go under `SPC o` prefix (Spacemacs convention for user-owned keys).

### Private Layers

Located in `home/.emacs.d/private/`:
- `gtd-zettelkasten/` — GTD workflow with org-roam (symlink to external repo)
- `rk-layout/` — Frame geometry persistence

## exec-path

Includes both Homebrew and mise shims:
- `/opt/homebrew/bin`
- `~/.local/share/mise/shims`

## After Changes

Emacs runs as daemon. Remind user to:
- Restart: `SPC q r`
- Reload config: `SPC f e R`
