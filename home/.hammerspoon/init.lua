-- helpful variable defs
local hyper = {"cmd","alt","ctrl","shift"}
local magic = {"cmd","alt","ctrl"}
local application = hs.application
local hotkey = hs.hotkey

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


hotkey.bind(hyper, "i", appLauncher('iTerm'))
hotkey.bind(hyper, "k", appLauncher('Google Chrome'))
hotkey.bind(hyper, "j", appLauncher('MacVim'))
hotkey.bind(hyper, "o", appLauncher('Slack'))

hs.alert.show("Config Loaded")
