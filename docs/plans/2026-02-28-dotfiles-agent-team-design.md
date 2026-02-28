# Dotfiles Agent Team Design

## Problem

Working with this dotfiles repo involves literate config editing, cross-config consistency, validation, and discovery across Hammerspoon, Spacemacs, shell, and tmux. Common pain points:

- Easy to break noweb references or tangle targets when editing .org files
- Keybindings span 5+ systems with no unified index
- No automated way to catch duplicate bindings, orphaned refs, or stale generated files
- Hard to rediscover what's configured where across sessions

## Approach

Skill-heavy design: 6 focused skills encoding domain knowledge, 3 agents that orchestrate them, and 2 generated reference docs for discovery.

## Skills

### 1. `literate-config`

Safe editing of .org literate config files with automatic tangling and verification.

**Domain knowledge:**
- Source-of-truth files: `dotspacemacs.org` -> `init.el`, `init.org` -> `init.lua`, `menuHammerCustomConfig.org` -> `menuHammerCustomConfig.lua`, `qutebrowser_config.org` -> `config.py`
- Never edit generated files
- Tangle command: `emacs --batch -l org --eval '(org-babel-tangle-file "FILE")'`
- Noweb syntax: `:noweb-ref config-layers`, `:noweb-ref user-config`
- Tangle targets: `:tangle`, `:tangle no`

**Enforced workflow:**
1. Read the .org source
2. Make the edit in the .org file
3. Tangle
4. Diff/verify the generated file contains the expected change
5. If Hammerspoon, remind to reload

### 2. `keybinding-registry`

Generates and maintains `docs/keybindings.md` — unified keybinding index.

**Sources:**
- `home/.hammerspoon/init.org` — hyper/magic/meh bindings
- `home/dotspacemacs.org` — SPC-prefixed and custom bindings
- `home/.zshrc` — shell keybindings
- `home/.tmux.conf.local` — tmux bindings
- `home/.config/karabiner/karabiner.json` — hardware remappings

**Output:** Markdown tables grouped by system with columns: Modifier, Key, Action, Source File, Line.

### 3. `config-inventory`

Generates and maintains `docs/config-map.md` — living map of what's configured where.

**Tracks:**
- All managed config files and purposes
- Literate source -> generated file relationships
- Cross-system dependencies (Hammerspoon -> emacsclient, shell -> pass, etc.)
- Scripts in `bin/` and `.local/bin/`
- Private Emacs layers

### 4. `hammerspoon-config`

Domain expertise for Hammerspoon changes.

**Knowledge:**
- Modifier conventions: `hyper` (all 4), `magic` (no shift), `meh` (no cmd)
- App launcher pattern: `hs.hotkey.bind(hyper, key, function() ... end)`
- GTD integration: `emacsclient -e '(org-...)'` via magic bindings
- Spoon installation via SpoonInstall
- Hotkey help overlay (`hyper+h`) should be updated when bindings change
- Source is `init.org`, never `init.lua`

Delegates to `literate-config` for edit/tangle cycle.

### 5. `spacemacs-config`

Domain expertise for Spacemacs/Emacs changes.

**Knowledge:**
- Noweb assembly: `:noweb-ref config-layers` blocks collect into `dotspacemacs-configuration-layers`
- Layer variables set in layer declaration, not user-config
- Additional packages in `dotspacemacs-additional-packages`
- Private layers in `home/.emacs.d/private/`
- Keybinding convention: `SPC o` prefix for user bindings
- Source is `dotspacemacs.org`, never `init.el`

Delegates to `literate-config` for edit/tangle cycle.

### 6. `config-validator`

Validation rules to catch common mistakes.

**Checks:**
- Duplicate keybindings across systems (same modifier+key in multiple places)
- Orphaned noweb references (ref used but no block, or block with unused ref)
- Generated files newer than source (direct edits to generated files)
- Untangled changes (source modified but generated file doesn't match)
- Broken cross-references (Hammerspoon calling nonexistent emacsclient functions)

**Output:** Issues list with severity (error/warning) and file:line references.

## Agents

### 1. `dotfiles-editor`

**Type:** Full-capability (Read, Write, Edit, Bash, Glob, Grep)

Primary agent for config changes. When asked to "add a Hammerspoon binding" or "add a Spacemacs layer," this agent:

1. Determines which config domain the change touches
2. Invokes the domain skill (`hammerspoon-config` or `spacemacs-config`)
3. Uses `literate-config` for all .org editing
4. Runs `config-validator` after changes
5. Updates keybinding registry and config inventory if affected

### 2. `dotfiles-auditor`

**Type:** Read-only (Read, Glob, Grep)

Generates reference docs and reports problems:

- Runs `config-validator` checks
- Regenerates `docs/keybindings.md` via `keybinding-registry`
- Regenerates `docs/config-map.md` via `config-inventory`
- Answers discovery questions ("where is X configured?")

Use after a batch of changes, at session start, or for lookups.

### 3. `dotfiles-tangler`

**Type:** Bash-only specialist

Lightweight tangle-and-verify agent:

1. Takes a list of .org files to tangle
2. Runs `emacs --batch` tangle commands
3. Diffs old vs new generated output
4. Reports what changed

Building block for the other agents.

## File Layout

```
.claude/skills/
├── literate-config/SKILL.md
├── keybinding-registry/SKILL.md
├── config-inventory/SKILL.md
├── hammerspoon-config/SKILL.md
├── spacemacs-config/SKILL.md
└── config-validator/SKILL.md

.claude/agents/
├── dotfiles-editor.md
├── dotfiles-auditor.md
└── dotfiles-tangler.md

docs/
├── keybindings.md      # Generated by keybinding-registry
└── config-map.md       # Generated by config-inventory
```

## Conventions

- Skills reference each other by name
- Agents declare which skills they use
- `dotfiles-editor` is the default for config change requests
- `dotfiles-auditor` is invoked via `/audit` command or at session start
- Generated docs are git-tracked and committed alongside config changes
