# Dotfiles Agent Team Design

## Problem

Working with this dotfiles repo involves literate config editing, cross-config consistency, validation, and discovery across Hammerspoon, Spacemacs, shell, and tmux. Common pain points:

- Easy to break noweb references or tangle targets when editing .org files
- Keybindings span 5+ systems with no unified index
- No automated way to catch duplicate bindings, orphaned refs, or stale generated files
- Hard to rediscover what's configured where across sessions

## Approach

Lean design: 4 focused skills encoding domain knowledge, 1 agent that orchestrates them. Reference docs are generated on demand rather than tracked as artifacts that drift.

## Skills

### 1. `literate-config`

Safe editing of .org literate config files with automatic tangling and verification.

**Domain knowledge:**
- Source-of-truth mappings:
  - `home/dotspacemacs.org` → `home/.spacemacs.d/init.el`
  - `home/.hammerspoon/init.org` → `home/.hammerspoon/init.lua`
  - `home/.hammerspoon/menuHammerCustomConfig.org` → `menuHammerCustomConfig.lua`
- Never edit generated files (they contain "DO NOT EDIT" warnings)
- Tangle command: `emacs --batch -l org --eval '(org-babel-tangle-file "FILE")'`
- Noweb syntax: `:noweb-ref config-layers`, `:noweb-ref user-config`, `:noweb-ref additional-packages`
- Tangle targets: `:tangle`, `:tangle no`

**Enforced workflow:**
1. Read the .org source
2. Make the edit in the .org file
3. Tangle
4. Verify the generated file contains the expected change (grep or diff)
5. If Hammerspoon config, remind user to reload (`hs.reload()` or hyper+r)

### 2. `hammerspoon-config`

Domain expertise for Hammerspoon changes. References `literate-config` workflow for all edits.

**Knowledge:**
- Modifier conventions: `hyper` (cmd+ctrl+alt+shift), `magic` (cmd+ctrl+alt), `meh` (ctrl+alt+shift)
- App launcher pattern: `hs.hotkey.bind(hyper, key, function() ... end)`
- GTD integration: `emacsclient -e '(org-...)'` via magic bindings
- Spoon installation via SpoonInstall
- Hotkey help overlay (`hyper+h`) auto-generates from bound keys — no manual update needed
- Source is `init.org`, never `init.lua`

### 3. `spacemacs-config`

Domain expertise for Spacemacs/Emacs changes. References `literate-config` workflow for all edits.

**Knowledge:**
- Noweb assembly: `:noweb-ref config-layers` blocks collect into `dotspacemacs-configuration-layers`
- Layer variables set in the layer declaration, not in user-config
- Additional packages via `:noweb-ref additional-packages`
- Private layers in `home/.emacs.d/private/`
- Keybinding convention: `SPC o` prefix for user bindings
- Emacs runs as daemon — changes require `SPC q r` (restart) or `SPC f e R` (reload config)
- Source is `dotspacemacs.org`, never `init.el`
- exec-path includes homebrew (`/opt/homebrew/bin`) and mise shims (`~/.local/share/mise/shims`)

### 4. `config-validator`

Validation rules to catch common mistakes. Run after changes or on demand.

**Checks:**
- Duplicate keybindings across systems (same modifier+key in Hammerspoon, Spacemacs, tmux, etc.)
- Orphaned noweb references (`:noweb-ref` used in assembly but no source block defines it, or vice versa)
- Generated files newer than source (sign of direct edits to generated files)
- Untangled changes (source .org modified more recently than generated output)
- Broken cross-references (Hammerspoon calling nonexistent emacsclient functions)

**Sources to scan:**
- `home/.hammerspoon/init.org` — hyper/magic/meh bindings
- `home/dotspacemacs.org` — SPC-prefixed and custom Emacs bindings
- `home/.zshrc` — shell keybindings (bindkey)
- `home/.tmux.conf.local` — tmux bindings
- `home/.config/karabiner/karabiner.json` — hardware remappings

**Output:** Issues list with severity (error/warning) and file:line references.

## Agent

### `dotfiles-editor`

**Type:** Full-capability (Read, Write, Edit, Bash, Glob, Grep)

Single agent for all dotfiles work — editing, auditing, and discovery. Behavior adapts to the request:

**Config changes** ("add a Hammerspoon binding", "add a Spacemacs layer"):
1. Determine which config domain the change touches
2. Follow the domain skill (`hammerspoon-config` or `spacemacs-config`)
3. Use `literate-config` workflow for all .org editing
4. Run `config-validator` checks after changes

**Auditing** ("check for problems", "validate my config"):
1. Run `config-validator` checks across all config domains
2. Report issues with file:line references

**Discovery** ("where is X configured?", "list all keybindings", "what uses hyper+g?"):
1. Search across all config sources (grep/glob)
2. Report findings with file:line references
3. For keybinding queries, scan all 5 binding sources and report as a table

## File Layout

```
.claude/skills/
├── literate-config/SKILL.md
├── hammerspoon-config/SKILL.md
├── spacemacs-config/SKILL.md
└── config-validator/SKILL.md

.claude/agents/
└── dotfiles-editor.md
```

## Conventions

- Skills reference each other by name in their instructions
- The agent reads relevant skills based on the task at hand
- `dotfiles-editor` is the default agent for any dotfiles config work
- Reference docs (keybinding index, config map) are generated on demand to stdout, not tracked as files
- Validation runs automatically after config changes, or on explicit request
