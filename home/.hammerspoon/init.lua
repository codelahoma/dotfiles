-- helpful variable defs
local hyper = {"cmd","alt","ctrl","shift"}
local magic = {"cmd","alt","ctrl"}
local application = hs.application
local hotkey = hs.hotkey
local grid = hs.grid
local window = hs.window
local screen = hs.screen
local spotify = hs.spotify

grid.setGrid('7x5')
-- grid.setGrid('4x4','1920x1200')
grid.ui.textSize = 100
grid.HINTS = {
  {'a','b','c','d','e','f', '1'},
  {'g','h','i','j','k','l', '2'},
  {'m','n','o','p','q','r', '8'},
  {'s','t','u','v','w','x','9'},
  {'y','z',';',',','.','0','\\'}
}
hotkey.bind(hyper, '8', grid.show)

hs.loadSpoon("ReloadConfiguration")
table.insert(spoon.ReloadConfiguration.watch_paths, '~/.homesick/repos/dotfiles/home/.hammerspoon')
spoon.ReloadConfiguration:start()

-- Music Controls
hotkey.bind(magic, 'space', spotify.displayCurrentTrack)
hotkey.bind(magic, 'p', spotify.playpause)
hotkey.bind(magic, 'n', function() spotify.next(); spotify.displayCurrentTrack() end)
hotkey.bind(magic, 'b', function() spotify.previous(); spotify.displayCurrentTrack() end)


-- -- What was I doing here?
-- function applicationWatcher(appName, eventType, appObject)
--   if (eventType == application.watcher.activated) then
--     if (appName == "Finder") then
--       -- Bring all Finder windows forward when one gets activated
--       appObject:selectMenuItem({"Window", "Bring All to Front"})
--     end
--   end
-- end
-- local appWatcher = application.watcher.new(applicationWatcher)
-- appWatcher:start()


local caffeine = hs.menubar.new()

local function setCaffeineDisplay(state)
  if state then
    caffeine:setTitle("AWAKE")
  else
    caffeine:setTitle("SLEEPY")
  end
end

local function caffeineClicked()
  setCaffeineDisplay(hs.caffeinate.toggle("displayIdle"))
end

if caffeine then
  caffeine:setClickCallback(caffeineClicked)
  setCaffeineDisplay(hs.caffeinate.get("displayIdle"))
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

local function focusMail()
  hs.window.find('Outlook Web App'):focus()
end

hotkey.bind(hyper, "d", appLauncher('Firefox Developer Edition'))
hotkey.bind(hyper, "e", appLauncher('Finder'))
hotkey.bind(hyper, "f", appLauncher('Firefox'))
hotkey.bind(hyper, "g", appLauncher('JIRA SI Board'))
hotkey.bind(hyper, "i", appLauncher('iTerm'))
hotkey.bind(hyper, "j", appLauncher('/usr/local/opt/emacs-plus/Emacs.app'))
hotkey.bind(hyper, "k", appLauncher('Google Chrome'))
hotkey.bind(hyper, "l", appLauncher('LibreOffice'))
hotkey.bind(hyper, "m", focusMail)
hotkey.bind(hyper, "n", appLauncher('Messages'))
hotkey.bind(hyper, "o", appLauncher('Slack'))
hotkey.bind(hyper, "p", appLauncher('Postman'))
hotkey.bind(hyper, "s", appLauncher('Skype for Business'))
hotkey.bind(hyper, "t", appLauncher('Tweetbot'))
hotkey.bind(hyper, "0", centerOnMainDisplay)
hotkey.bind(hyper, "1", appLauncher('1Password 7'))
hotkey.bind(hyper, ";", appLauncher('Spotify'))

hs.alert.show("Config Loaded")
