---
name: hammerspoon-config
description: Use when working with Hammerspoon configuration, hotkeys, hyper/magic/meh bindings, app launchers, spoons, or macOS automation via Hammerspoon.
---

# Hammerspoon Config

## Overview

Hammerspoon macOS automation configured via literate config in `home/.hammerspoon/init.org`. Source is `init.org`, never `init.lua`.

**REQUIRED:** Use `literate-config` skill for all edits.

## Modifier Conventions

| Name | Keys | Usage |
|---|---|---|
| `hyper` | cmd+ctrl+alt+shift | App launchers, primary bindings |
| `magic` | cmd+ctrl+alt | GTD/Emacs integration |
| `meh` | ctrl+alt+shift | Secondary bindings |

## Patterns

### App Launcher

```lua
hs.hotkey.bind(hyper, "key", function()
    hs.application.launchOrFocus("AppName")
end)
```

### GTD Integration (magic bindings)

```lua
hs.hotkey.bind(magic, "key", function()
    hs.execute("emacsclient -e '(org-capture nil \"t\")'", true)
end)
```

### SpoonInstall

```lua
hs.loadSpoon("SpoonInstall")
spoon.SpoonInstall:andUse("SpoonName", { config = { ... } })
```

## Key Facts

- Hotkey help overlay (`hyper+h`) auto-generates from bound keys — no manual update needed
- MenuHammer config is separate: `menuHammerCustomConfig.org`
- After changes, remind user to reload: `hs.reload()` or `hyper+r`
