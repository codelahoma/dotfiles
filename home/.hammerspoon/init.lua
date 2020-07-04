-- helpful variable defs
local hyper = {"cmd","alt","ctrl","shift"}
local magic = {"cmd","alt","ctrl"}
local application = hs.application
local hotkey = hs.hotkey
local grid = hs.grid
local window = hs.window
local screen = hs.screen
local spotify = hs.spotify

grid.setGrid('9x3')
grid.setMargins({10,10})
-- grid.setGrid('4x4','1920x1200')
grid.ui.textSize = 100
-- grid.HINTS = {
--   {'a','b','c','d','e','f', '1'},
--   {'g','h','i','j','k','l', '2'},
--   {'m','n','o','p','q','r', '8'},
--   {'s','t','u','v','w','x','9'},
--   {'y','z',';',',','.','0','='}
-- }
hotkey.bind(hyper, '8', grid.show)

hs.hints.style = 'vimperator'
hotkey.bind(hyper, '9', hs.hints.windowHints)

hs.loadSpoon("ReloadConfiguration")
table.insert(spoon.ReloadConfiguration.watch_paths, '~/.homesick/repos/dotfiles/home/.hammerspoon')
spoon.ReloadConfiguration:start()

local anycomplete = require "anycomplete/anycomplete"
anycomplete.registerDefaultBindings()

hs.loadSpoon("SpoonInstall")
spoon.SpoonInstall.use_syncinstall = true

Install = spoon.SpoonInstall

Install:andUse("MouseCircle",
               {
                 config = {
                   color = hs.drawing.color.osx_red,
                 },
                 hotkeys = {
                   show = { hyper, "b" }
}})

spoon.SpoonInstall:andUse("Seal",
                          { hotkeys = { show = { hyper, "u" } },
                            fn = function(s)
                              s:loadPlugins({"apps", "calc", "safari_bookmarks", "useractions"})
                              s.plugins.useractions.actions = {
                                ["Restart Hammerspoon"] = { fn = hs.reload },
                                ["Pick me up"] = {
                                  fn = hs.fnutils.partial(hs.alert.show, "Looking fine today!")
                                },
                                ["Hammerspoon docs webpage"] = {
                                  url = "http://hammerspoon.org/docs/",
                                  icon = hs.image.imageFromName(hs.image.systemImageNames.ApplicationIcon)
                                }
                              }
                            end,
                            start = true,
})

hotkey.bind(magic, 'space', spotify.displayCurrentTrack)
-- hotkey.bind(magic, 'p', spotify.playpause)
-- hotkey.bind(magic, 'n', function() spotify.next(); spotify.displayCurrentTrack() end)
-- hotkey.bind(magic, 'b', function() spotify.previous(); spotify.displayCurrentTrack() end)

-- Install:andUse("KSheet",
--                {
--                  hotkeys = {
--                    toggle = { hyper, "/"}
-- }})
function initKSheet()
  Install:andUse('KSheet')
  local shouldShow = true

  function toggleKSheet()
    if shouldShow then
      spoon.KSheet:show()
      shouldShow = false 
    else
      spoon.KSheet:hide()
      shouldShow = true
    end
  end

  return toggleKSheet
end

local ksheet = initKSheet()

modal = hs.hotkey.modal.new(hyper, "n", " Going Modal! ")

-- in this example, Ctrl+Shift+h triggers this keybinding mode, which will allow us to use the ones defined below. A nice touch for usability: This also offers to show a message.

-- I recommend having this one at all times: Bind the escape key to exit keybinding mode:
modal:bind("", "escape", " not this time...", nil, function() modal:exit() end, nil)

-- An example binding I find useful: Type today's date in ISO format.
-- modal:bind("", "d", "today", nil, function() hs.eventtap.keyStrokes(os.date("%F")) modal:exit() end, nil)
modal:bind("", "a", "activity", nil, function() application.launchOrFocus("Activity Monitor") modal:exit() end, nil)
modal:bind("", "d", "dash", nil, function() application.launchOrFocus("Dash") modal:exit() end, nil)
modal:bind("", "e", "excel", nil, function() application.launchOrFocus("Excel") modal:exit() end, nil)
modal:bind("", "m", "menu", nil, function() ksheet() modal:exit() end, nil)
modal:bind("", "p", "postman", nil, function() application.launchOrFocus("Postman") modal:exit() end, nil)
modal:bind("", "s", "spark", nil, function() application.launchOrFocus("Spark") modal:exit() end, nil)
modal:bind("", "v", "paste", nil, function() hs.eventtap.keyStroke({"cmd", "shift"}, "v") modal:exit() end, nil)

caffeine = hs.menubar.new()
hs.caffeinate.set("system", true, false)

local function setCaffeineDisplay(state)
  if state then
    caffeine:setIcon("caffeine-on.pdf")
  else
    caffeine:setIcon("caffeine-off.pdf")
  end
end

local function caffeineClicked()
  setCaffeineDisplay(hs.caffeinate.toggle("system"))
end

if caffeine then
  caffeine:setClickCallback(caffeineClicked)
  setCaffeineDisplay(hs.caffeinate.get("system"))
end

-- center current window on big screen, if present
local function centerOnMainDisplay()
  local bigScreen = screen.find('LG Ultra HD')
  if bigScreen then
    window.focusedWindow():centerOnScreen(bigScreen)
  end
end

-- app launchers
local function appLauncher(app)
  return function()
    application.launchOrFocus(app)
  end
end

local function pasteLauncher()
  return function()
    hs.eventtap.keyStroke({"cmd", "shift"}, "v")
  end
end

hotkey.bind(hyper, "b", appLauncher('Kindle'))
hotkey.bind(hyper, "c", hs.toggleConsole)
hotkey.bind(hyper, "d", appLauncher('Dash'))
hotkey.bind(hyper, "e", appLauncher('Finder'))
hotkey.bind(hyper, "f", appLauncher('Firefox'))
hotkey.bind(hyper, "h", appLauncher('VMware Horizon Client'))
hotkey.bind(hyper, "i", appLauncher('iTerm'))
hotkey.bind(hyper, "j", appLauncher('/usr/local/opt/emacs-plus@27/Emacs.app'))
hotkey.bind(hyper, "k", appLauncher('Google Chrome'))
-- hotkey.bind(hyper, "l", appLauncher('LibreOffice'))
hotkey.bind(hyper, "m", appLauncher('Microsoft Edge'))
-- hotkey.bind(hyper, "n", appLauncher('Messages'))
hotkey.bind(hyper, "o", appLauncher('Slack'))
hotkey.bind(hyper, "p", appLauncher('Pycharm'))

hotkey.bind(hyper, "q", appLauncher('qutebrowser'))
hotkey.bind(hyper, "r", hs.reload)
-- hotkey.bind(hyper, "s", appLauncher('Stickies'))
hotkey.bind(hyper, "s", appLauncher('Skype for Business'))
-- hotkey.bind(hyper, "t", appLauncher('Microsoft Teams'))
-- hotkey.bind(hyper, "u", appLauncher('Microsoft OneNote'))
hotkey.bind(hyper, "v", pasteLauncher())
-- hotkey.bind(hyper, "u", appLauncher('Visual Studio Code - Insiders'))
hotkey.bind(hyper, "0", centerOnMainDisplay)
hotkey.bind(hyper, "1", appLauncher('1Password 7'))
hotkey.bind(hyper, ";", appLauncher('Spotify'))
hotkey.bind(hyper, "return", appLauncher('OmniFocus'))

local localfile = hs.configdir .. "/init-local.lua"
if hs.fs.attributes(localfile) then
  dofile(localfile)
end

hs.ipc.cliInstall()
hs.alert.show("Config Loaded")
