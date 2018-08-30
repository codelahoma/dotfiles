-- helpful variable defs
local hyper = {"cmd","alt","ctrl","shift"}
local magic = {"cmd","alt","ctrl"}
local application = hs.application
local hotkey = hs.hotkey
local grid = hs.grid

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
local spotify = hs.spotify
hotkey.bind(magic, 'space', spotify.displayCurrentTrack)
hotkey.bind(magic, 'p', spotify.playpause)
hotkey.bind(magic, 'n', function() spotify.next(); spotify.displayCurrentTrack() end)
hotkey.bind(magic, 'b', function() spotify.previous(); spotify.displayCurrentTrack() end)


function applicationWatcher(appName, eventType, appObject)
  if (eventType == application.watcher.activated) then
    if (appName == "Finder") then
      -- Bring all Finder windows forward when one gets activated
      appObject:selectMenuItem({"Window", "Bring All to Front"})
    end
  end
end
appWatcher = application.watcher.new(applicationWatcher)
appWatcher:start()


caffeine = hs.menubar.new()
function setCaffeineDisplay(state)
  if state then
    caffeine:setTitle("AWAKE")
  else
    caffeine:setTitle("SLEEPY")
  end
end

function caffeineClicked()
  setCaffeineDisplay(hs.caffeinate.toggle("displayIdle"))
end

if caffeine then
  caffeine:setClickCallback(caffeineClicked)
  setCaffeineDisplay(hs.caffeinate.get("displayIdle"))
end

-- mouse highlighting
mouseCircle = nil
mouseCircleTimer = nil

function mouseHighlight()
  -- Delete an existing highlight if it exists
  if mouseCircle then
    mouseCircle:delete()
    if mouseCircleTimer then
      mouseCircleTimer:stop()
    end
  end

  -- Get the current coordinates of the mouse pointer
  mousepoint = hs.mouse.getAbsolutePosition()
  -- Prepare a big red circle around the mouse pointer
  mouseCircle = hs.drawing.circle(hs.geometry.rect(mousepoint.x-40, mousepoint.y-40, 80, 80))
  mouseCircle:setStrokeColor({["red"]=1,["blue"]=0,["green"]=0,["alpha"]=1})
  mouseCircle:setFill(false)
  mouseCircle:setStrokeWidth(5)
  mouseCircle:show()

  -- set a timer to delete the circle after 3 seconds
  mouseCircleTimer = hs.timer.doAfter(3, function() mouseCircle:delete() end)
end
hotkey.bind(hyper, "D", mouseHighlight)

-- app launchers
local function appLauncher(app)
  return function()
    application.launchOrFocus(app)
  end
end


hotkey.bind(hyper, "e", appLauncher('Finder'))
hotkey.bind(hyper, "f", appLauncher('JIRA SI Board'))
hotkey.bind(hyper, "i", appLauncher('iTerm'))
hotkey.bind(hyper, "j", appLauncher('/usr/local/opt/emacs-plus/Emacs.app'))
hotkey.bind(hyper, "k", appLauncher('Google Chrome'))
hotkey.bind(hyper, "l", appLauncher('LibreOffice'))
hotkey.bind(hyper, "m", appLauncher('Mail'))
hotkey.bind(hyper, "o", appLauncher('Slack'))
hotkey.bind(hyper, "p", appLauncher('Postman'))
hotkey.bind(hyper, ";", appLauncher('Spotify'))
hotkey.bind(hyper, "1", appLauncher('1Password 6'))
hotkey.bind(hyper, "s", appLauncher('Skype for Business'))

hs.alert.show("Config Loaded")
