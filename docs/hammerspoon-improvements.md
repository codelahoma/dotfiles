# Hammerspoon Configuration Review

## Current State Summary

Your configuration is well-organized with literate programming via org-mode. Key features include:

- **Modifier keys**: `hyper`, `magic`, and `meh` combinations
- **Spoon management**: SpoonInstall with custom MenuHammer repo
- **App launching**: Tracked statistics with graphical display
- **Window management**: WindowGrid with custom hints
- **URL dispatching**: Routes URLs to appropriate browsers
- **Emacs integration**: editWithEmacs spoon
- **Caffeinate**: Menu bar toggle for preventing sleep

---

## Suggested Improvements

### 1. Window Management Enhancements

The commented-out "Josh's Window functions" section could be revived with improvements:

```lua
-- Quick window snapping without grid overlay
local function snapLeft()
   hs.window.focusedWindow():moveToUnit({0, 0, 0.5, 1})
end

local function snapRight()
   hs.window.focusedWindow():moveToUnit({0.5, 0, 0.5, 1})
end

-- Cycle through window sizes (thirds)
local function cycleWindowSize()
   -- Track current size and cycle 1/3 -> 1/2 -> 2/3 -> full
end
```

**Benefit**: Faster than grid overlay for common operations.

---

### 2. Application Context Layouts

Auto-arrange windows based on workflow context:

```lua
local layouts = {
   coding = {
      {"Emacs", nil, screen.mainScreen(), hs.layout.left50, nil, nil},
      {"iTerm2", nil, screen.mainScreen(), hs.layout.right50, nil, nil},
   },
   writing = {
      {"Emacs", nil, screen.mainScreen(), hs.layout.maximized, nil, nil},
   },
   research = {
      {"Arc", nil, screen.mainScreen(), hs.layout.left70, nil, nil},
      {"DEVONthink 3", nil, screen.mainScreen(), hs.layout.right30, nil, nil},
   }
}

hotkey.bind(hyper, "1", function() hs.layout.apply(layouts.coding) end)
hotkey.bind(hyper, "2", function() hs.layout.apply(layouts.writing) end)
hotkey.bind(hyper, "3", function() hs.layout.apply(layouts.research) end)
```

**Benefit**: One keystroke to set up your workspace for different tasks.

---

### 3. Focus Mode / Do Not Disturb Integration

```lua
local focusMode = false

local function toggleFocusMode()
   focusMode = not focusMode
   if focusMode then
      -- Enable DND
      hs.execute("shortcuts run 'Focus On'")
      -- Quit distracting apps
      local distractions = {"Slack", "Discord", "Spark Mail"}
      for _, app in ipairs(distractions) do
         local a = hs.application.get(app)
         if a then a:hide() end
      end
      hs.alert.show("Focus Mode ON")
   else
      hs.execute("shortcuts run 'Focus Off'")
      hs.alert.show("Focus Mode OFF")
   end
end

hotkey.bind(hyper, "escape", toggleFocusMode)
```

**Benefit**: Instantly eliminate distractions for deep work sessions.

---

### 4. WiFi-Based Automation

```lua
local wifiWatcher = hs.wifi.watcher.new(function()
   local network = hs.wifi.currentNetwork()
   if network == "HomeNetwork" then
      -- Home setup: external monitor layouts, personal apps
      hs.alert.show("Welcome Home")
   elseif network == "WorkNetwork" then
      -- Work setup: VPN, work apps
      hs.alert.show("Work Mode")
      hs.application.launchOrFocus("Slack")
   end
end)
wifiWatcher:start()
```

**Benefit**: Automatic context switching when you change locations.

---

### 5. USB Device Watcher

```lua
local usbWatcher = hs.usb.watcher.new(function(data)
   if data.eventType == "added" then
      if data.productName:match("YubiKey") then
         hs.alert.show("YubiKey connected")
      elseif data.productName:match("Dygma") then
         -- Keyboard connected - maybe reload specific config
         hs.alert.show("Dygma Defy connected")
      end
   end
end)
usbWatcher:start()
```

**Benefit**: React to hardware changes automatically.

---

### 6. Quick Capture to Emacs/Org

```lua
local function quickCapture()
   -- Use emacsclient to trigger org-capture
   hs.execute("/opt/homebrew/bin/emacsclient -e '(org-capture nil \"i\")'")
   hs.application.launchOrFocus("Emacs")
end

hotkey.bind(magic, "n", quickCapture)
```

**Benefit**: System-wide quick capture without leaving current context.

---

### 7. Clipboard History with Preview

The `pasteLauncher()` function could be enhanced:

```lua
Install:andUse("ClipboardTool", {
   config = {
      show_in_menubar = false,
      hist_size = 100,
   },
   hotkeys = {
      toggle_clipboard = { hyper, "v" }
   },
   start = true,
})
```

**Benefit**: Visual clipboard history instead of relying on external app.

---

### 8. Meeting Mode

```lua
local meetingMode = false

local function toggleMeetingMode()
   meetingMode = not meetingMode
   if meetingMode then
      -- Mute system audio notifications
      hs.execute("osascript -e 'set volume output muted true'")
      -- Close notification center
      hs.execute("killall NotificationCenter 2>/dev/null")
      -- Maybe auto-join if calendar has meeting link
      hs.alert.show("Meeting Mode ON")
   else
      hs.execute("osascript -e 'set volume output muted false'")
      hs.alert.show("Meeting Mode OFF")
   end
end
```

**Benefit**: One-click preparation for video calls.

---

### 9. Pomodoro Timer

```lua
local pomodoroTimer = nil
local pomodoroMinutes = 25

local function startPomodoro()
   if pomodoroTimer then
      pomodoroTimer:stop()
   end
   hs.alert.show("Pomodoro started: " .. pomodoroMinutes .. " minutes")
   pomodoroTimer = hs.timer.doAfter(pomodoroMinutes * 60, function()
      hs.alert.show("Pomodoro complete! Take a break.", 10)
      hs.sound.getByName("Glass"):play()
   end)
end

hotkey.bind(magic, "p", startPomodoro)
```

**Benefit**: Built-in productivity timer without external apps.

---

### 10. Screen/Display Automation

```lua
local function handleScreenChange()
   local screens = hs.screen.allScreens()
   if #screens == 1 then
      -- Laptop only - maybe different layout
      hs.alert.show("Single display mode")
   elseif #screens == 2 then
      -- External monitor connected
      hs.alert.show("Dual display mode")
      -- Auto-arrange apps to preferred screens
   end
end

hs.screen.watcher.new(handleScreenChange):start()
```

**Benefit**: Automatic layout adjustment when connecting/disconnecting displays.

---

### 11. Application-Specific Hotkeys

```lua
-- Arc-specific bindings
local arcWatcher = hs.application.watcher.new(function(name, event, app)
   if name == "Arc" and event == hs.application.watcher.activated then
      -- Enable Arc-specific hotkeys
   elseif name == "Arc" and event == hs.application.watcher.deactivated then
      -- Disable Arc-specific hotkeys
   end
end)
arcWatcher:start()
```

**Benefit**: Context-aware hotkeys that only work in specific apps.

---

### 12. Battery Alerts

```lua
local batteryWatcher = hs.battery.watcher.new(function()
   local pct = hs.battery.percentage()
   local charging = hs.battery.isCharging()

   if pct <= 20 and not charging then
      hs.alert.show("Low battery: " .. pct .. "%", 5)
   elseif pct >= 80 and charging then
      hs.alert.show("Battery charged: " .. pct .. "%", 3)
   end
end)
batteryWatcher:start()
```

**Benefit**: Never get caught with a dead battery.

---

## Quick Wins (Easy to Implement)

1. **Revive window snapping** - Uncomment and simplify Josh's functions
2. **Add Pomodoro timer** - ~15 lines of code
3. **Add battery watcher** - ~10 lines of code
4. **Add screen change handler** - Useful with LG HDR 4K reference in code

## Medium Effort

1. **Application layouts** - Requires defining your workflow patterns
2. **Focus mode** - Needs macOS Shortcuts integration
3. **Quick capture to Org** - Already have Emacs integration

## Higher Effort

1. **WiFi automation** - Requires identifying network names and behaviors
2. **Meeting mode** - Integrate with calendar for auto-detection

---

## Code Quality Suggestions

1. **Modularize**: Split into separate files (`window-management.lua`, `app-launchers.lua`, etc.)
2. **Error handling**: Add pcall wrappers around external commands
3. **Consistent naming**: Mix of camelCase and snake_case in variables
4. **Remove dead code**: Large commented sections could move to a `deprecated.lua` file

---

*Generated for Hammerspoon config review - January 2026*
