-- Making the Runtime, Funtime with Hammerspoon
-- https://blog.theodo.fr/2018/03/making-runtime-funtime-hammerspoon/
--
-- Just Enough Lua to Be Productive in Hammerspoon, Part 1
-- http://zzamboni.org/post/just-enough-lua-to-be-productive-in-hammerspoon-part-1/
--
-- My Hammerspoon configuration with commentary
-- http://zzamboni.org/post/my-hammerspoon-configuration-with-commentary/

local log = hs.logger.new('hs/init.lua', 'info')
local hyper = { "cmd", "ctrl" }


-----------------------------------------------
-- Reload config on write
-----------------------------------------------

log.i("Setting up spoons")
hs.loadSpoon("ReloadConfiguration")
spoon.ReloadConfiguration:start()
-- hs.alert.show("Config loaded")
hs.notify.new({ title = "Hammerspoon", informativeText = "Config loaded" }):send()
hs.alert.show("Hammerspoon: config loaded")


hs.loadSpoon("SpoonInstall")
-- Add official github Spoons repos
spoon.SpoonInstall.repos = {
  default = {
    url = "https://github.com/Hammerspoon/Spoons",
    desc = "Main Hammerspoon Spoon Repository",
  }
}

-- Update default Spoons repos
spoon.SpoonInstall:updateRepo()

local spoons_list = {
  -- "BrewInfo",
  "AClock",
  "Calendar",
  "CountDown",
  "EmmyLua",
  "KSheet",
  "HSKeybindings",
  "Seal",
  "URLDispatcher",
  "WindowHalfsAndThirds",
  "WindowScreenLeftAndRight",
}

-- Install and load spoons
for _, _spoon in pairs(spoons_list) do
  if not hs.spoons.isInstalled(_spoon) then
    spoon.SpoonInstall:installSpoonFromRepo(_spoon)
  end
  hs.loadSpoon(_spoon)
end

hs.loadSpoon("AClock")
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "C", function()
  spoon.AClock:toggleShow()
end)


-- Edit Ghostty config using helix inside Ghostty

local modal = hs.hotkey.modal.new()
modal:bind({"command"}, ",", function()
-- Inside ghostty, if you set command, it implies "wait after command = true"
-- So work around by just running a command. It's slower, but exits cleanly
        hs.osascript.applescript([[
tell application "Ghostty"
	set cfg to new surface configuration
	set initial input of cfg to "/usr/bin/env hx /Users/aye011/.config/ghostty/config; exit" & linefeed
	set wait after command of cfg to false
	set win to new window with configuration cfg
end tell
          ]])
      end)
hs.window.filter.new('Ghostty')
  :subscribe(hs.window.filter.windowFocused,function()
    modal:enter()
  end)
  :unsubscribe(hs.window.filter.windowUnfocused,function()
    modal:exit()
  end)


-- How to serialise things between launches/restarts.
--
-- Questions:
-- - Are there global variables?
--   I think there are, globals from init.lua are global... I think.
-- - Do variables persist across config reloads?
--   ??? But should be easy enough to test.
--
-- Options for more persistant storage:
-- - hs.settings
-- - hs.sqlite3

-- Create a table and use os.date to add to it each time the config is reloaded. Then we can inspect it to see if it persists across reloads.
if not myConfigReloadTimes then
  myConfigReloadTimes = {}
end

table.insert(myConfigReloadTimes, os.date())
log.i("Config reload times: ", hs.inspect(myConfigReloadTimes))


-- local myCanvas = {}
-- Ref: https://github.com/brennydoogles/hammerspoon-multishade/blob/main/src/init.lua
function ShowTask()
  for i, v in ipairs(hs.screen.allScreens()) do
    log.i("ShowTask", i, hs.inspect(v))
  end
  local rect = hs.screen.allScreens()[1]:fullFrame() -- hs.geometry.rect(0.0,0.0,2560.0,1440.0)
  local myCanvas = hs.canvas.new(rect):appendElements( {
-- first we start with a rectangle that covers the full canvas
    -- action = "build",
    action = "fill",
    fillColor = {green = 0.5},
    padding = 0, type = "rectangle"
  }):alpha(0.5):show(0.5):canvasMouseEvents(true, nil, nil, nil):mouseCallback(function (canvas, event_type, canvas_element_id, x, y)
    if event_type == "mouseDown" then
      canvas:hide(0.5)
      myCanvas = nil
    end

  end )

end


-- hs.loadSpoon("SpoonInstall")
-- -- hs.loadSpoon("WindowHalfsAndThirds")
-- hs.loadSpoon("WindowScreenLeftAndRight")

-- spoon.WindowScreenLeftAndRight:bindHotkeys({
--     screen_left = { hyper, "Left" },
--     screen_right = { hyper, "Right" }
-- })

-- Enable IPC for using the `hs` CLI
require("hs.ipc")

log.i("Setting up window management")
-- See https://github.com/AaronLasseigne/dotfiles/blob/50d2325c1ad7552ea95a313fbf022004e2932ce9/.hammerspoon/init.lua
-- on branch 'master' it has been significantly updated
hs.window.animationDuration = 0

hs.hotkey.bind({ "cmd", "alt", "ctrl" }, "W", function()
  -- hs.alert.show("Hello World!")
  hs.notify.new({ title = "Hammerspoon", informativeText = "Hello World" }):send()
end)

-- Copy URL + Title
-- hs.hotkey.bind({"cmd", "shift"}, "Space", function ()
--     hs.application.frontmostApplication():focusedWindow()
--     local title = hs.window.focusedWindow():title()
--     hs.notify.new({title="Page Title", informativeText=title}):send()
--     hs.eventtap.keyStroke({"cmd"}, "l")
--     hs.eventtap.keyStroke({"cmd"}, "c")
--     hs.eventtap.keyStroke({}, "escape")
--     local url = hs.pasteboard.readString()
--     hs.notify.new({title="Page URL", informativeText=url}):send()
--     hs.pasteboard.setContents("[[" .. url .. "][" .. title .. "]]")
-- end)

-- Toggle a window between its normal size, and being maximized
local frameCache = {}
local function toggle_window_maximized()
  local win = hs.window.focusedWindow()
  if frameCache[win:id()] then
    win:setFrame(frameCache[win:id()])
    frameCache[win:id()] = nil
  else
    frameCache[win:id()] = win:frame()
    win:maximize()
  end
end

hs.hotkey.bind(hyper, "k", toggle_window_maximized)

hs.hotkey.bind(hyper, "y", hs.toggleConsole)

local positions = {
  maximized = hs.layout.maximized,
  centered = { x = 0.15, y = 0.15, w = 0.7, h = 0.7 },

  left34 = { x = 0, y = 0, w = 0.34, h = 1 },
  left50 = hs.layout.left50,
  left66 = { x = 0, y = 0, w = 0.66, h = 1 },

  right34 = { x = 0.66, y = 0, w = 0.34, h = 1 },
  right50 = hs.layout.right50,
  right66 = { x = 0.34, y = 0, w = 0.66, h = 1 },

  upper50 = { x = 0, y = 0, w = 1, h = 0.5 },
  upper50Left50 = { x = 0, y = 0, w = 0.5, h = 0.5 },
  upper50Right50 = { x = 0.5, y = 0, w = 0.5, h = 0.5 },

  lower50 = { x = 0, y = 0.5, w = 1, h = 0.5 },
  lower50Left50 = { x = 0, y = 0.5, w = 0.5, h = 0.5 },
  lower50Right50 = { x = 0.5, y = 0.5, w = 0.5, h = 0.5 }
}

-- hs.hotkey.bind(hyper, "k", function()
--                  local win = hs.window.focusedWindow()
--                  win:setFrame(win:screen():frame())

-- end)

local function bindKey(key, fn)
  hs.hotkey.bind(hyper, key, fn)
end

local grid = {
  { key = "u", units = { positions.upper50Left50 } },
  { key = "i", units = { positions.upper50 } },
  { key = "o", units = { positions.upper50Right50 } },

  { key = "j", units = { positions.left50, positions.left66, positions.left34 } },
  -- {key="k", units={positions.centered, positions.maximized}},
  { key = "l", units = { positions.right50, positions.right66, positions.right34 } },

  { key = "m", units = { positions.lower50Left50 } },
  -- Comment out comma, it conflicts with reload kitty config
  -- {key=",", units={positions.lower50}},
  { key = ".", units = { positions.lower50Right50 } }
}

hs.fnutils.each(grid, function(entry)
  bindKey(entry.key, function()
    local units = entry.units
    local screen = hs.screen.mainScreen()
    local window = hs.window.focusedWindow()
    local windowGeo = window:frame()

    local index = 0
    hs.fnutils.find(units, function(unit)
      index = index + 1

      local geo = hs.geometry.new(unit):fromUnitRect(screen:frame()):floor()
      return windowGeo:equals(geo)
    end)
    if index == #units then index = 0 end

    window:moveToUnit(units[index + 1])
  end)
end)

-----------------------------------------------
-- Watch Downloads folder
-----------------------------------------------
log.i("Setting up Downloads folder monitoring")
local downloadLog = hs.logger.new('DLMonitor', 'debug')

local downloadConfig = {
  folder = hs.fs.pathToAbsolute("~/Downloads"),
  rules = {
    {
      name = "torrent -> transmission",
      patterns = { "%.torrent$" },
      command = "/opt/homebrew/bin/transmission-remote",
      args = { "nixos", "--add", "$FILE" },
      onSuccess = "trash",
    },
  },
}

local downloadInFlight = {}

local function moveToTrash(path)
  hs.task.new("/usr/bin/trash", function(exitCode, stdOut, stdErr)
    if exitCode == 0 then
      downloadLog.i("Success moving " .. path .. " to the Trash.")
    else
      downloadLog.e("Error moving " .. path .. " to the Trash. " .. (stdOut or "") .. " " .. (stdErr or ""))
    end
  end, { "--stopOnError", "--verbose", path }):start()
end

local function isRegularFile(path)
  local attrs = hs.fs.attributes(path)
  return attrs and attrs.mode == "file"
end

local function matchesRule(fileName, rule)
  for _, pattern in ipairs(rule.patterns or {}) do
    if fileName:match(pattern) then
      return true
    end
  end
  return false
end

local function renderArgs(argsTemplate, fullPath, fileName)
  local args = {}
  for _, item in ipairs(argsTemplate or {}) do
    local rendered = item:gsub("%$FILE", fullPath):gsub("%$NAME", fileName)
    table.insert(args, rendered)
  end
  return args
end

local function runRule(rule, fileName, fullPath)
  if downloadInFlight[fullPath] then
    return
  end
  downloadInFlight[fullPath] = true

  if not hs.fs.attributes(rule.command) then
    downloadInFlight[fullPath] = nil
    downloadLog.e(string.format("[%s] command not found: %s", rule.name, tostring(rule.command)))
    return
  end

  local args = renderArgs(rule.args, fullPath, fileName)
  local task = hs.task.new(rule.command, function(exitCode, stdOut, stdErr)
    downloadInFlight[fullPath] = nil
    if exitCode == 0 then
      local msg = string.format("[%s] processed: %s", rule.name, fileName)
      hs.notify.show("HS Download Monitor", "Rule success", msg)
      downloadLog.i(msg)
      if rule.onSuccess == "trash" then
        moveToTrash(fullPath)
      end
    else
      local msg = string.format("[%s] failed (%d): %s\nstdout: %s\nstderr: %s", rule.name, exitCode, fileName,
        stdOut or "", stdErr or "")
      hs.notify.show("HS Download Monitor", "Rule failed", msg)
      downloadLog.e(msg)
    end
  end, args)

  if task then
    task:start()
  else
    downloadInFlight[fullPath] = nil
    downloadLog.e(string.format("[%s] failed to create task for %s", rule.name, fileName))
  end
end

local function processDownloads(reason)
  local attrs = hs.fs.attributes(downloadConfig.folder)
  if not attrs or attrs.mode ~= "directory" then
    downloadLog.e("Downloads folder is not accessible: " .. tostring(downloadConfig.folder))
    return
  end

  downloadLog.i("Processing Downloads (" .. reason .. ")")
  for file in hs.fs.dir(downloadConfig.folder) do
    if file ~= "." and file ~= ".." then
      local fullPath = downloadConfig.folder .. "/" .. file
      if isRegularFile(fullPath) then
        for _, rule in ipairs(downloadConfig.rules) do
          if matchesRule(file, rule) then
            runRule(rule, file, fullPath)
            break
          end
        end
      end
    end
  end
end

local function handleDownloadChange(paths, _)
  downloadLog.i("Downloads monitor triggered for " .. tostring(#paths) .. " changed paths")
  processDownloads("watcher")
end

-- Keep a global reference so Lua GC doesn't collect and stop the watcher.
downloadWatcher = hs.pathwatcher.new(downloadConfig.folder, handleDownloadChange)
downloadWatcher:start()
downloadLog.i("Watching Downloads folder: " .. downloadConfig.folder)

-- Run once on config load so existing files are processed too.
processDownloads("startup")



-----------------------------------------------
-- Hyper i to show window hints
-----------------------------------------------

-- hs.hotkey.bind(hyper, "i", function()
--   hs.hints.windowHints()
-- end)


-- hs.loadSpoon("RoundedCorners")
-- spoon.RoundedCorners:start()


-- Activate with hammerspoon://test?somParam=hello
-- For example, from Alfred
hs.urlevent.bind("test1", function(eventName, params)
  if params["someParam"] then
    hs.alert.show(params["someParam"])
  end
end)

-- https://github.com/
-- https://travis-ci.org/

-- require('wifi')
-- $ osascript -e 'id of app "Firefox"'
-- This are not regexps, they are lua patterns. See https://www.lua.org/pil/20.2.html
-- Be careful of - hyphens
-- -- spoon.SpoonInstall:andUse("URLDispatcher",
--                           -- {
--                             -- config = {
-- --                               url_patterns = {
-- --                                 { "https?://github%.com", "org.mozilla.firefox" },
-- --                                 { "https?://travis%-ci%.org", "org.mozilla.firefox" },
-- --                                 { "https?://gaautobots%.atlassian%.net/", "org.mozilla.firefox" },
-- --                                 { "https?://.*%.office%.com/", "org.mozilla.firefox" },
--   --                             },
--                               default_handler = "com.apple.Safari"
--                             },
--                             start = true
--                           }
-- )


--[[
Hammerspoon script for CONTEXT-AWARE hotkeys in Microsoft Outlook.
- Only remaps keys when not focused on a text input field.
--]]

-- 1. Define the target application
local outlookBundleID = "com.microsoft.Outlook"

-- 2. Helper function to check if the user is in a text input field
-- When composing an email, the body is a web view (AXWebArea).
-- This function checks the focused element and its parents for text-editing roles.
function isTypingMode()
  local element = hs.axui.focusedElement()
  if not element then return false end

  -- List of roles indicating a text input area
  local textInputRoles = {
    ["AXTextArea"] = true,
    ["AXTextField"] = true,
    ["AXWebArea"] = true,     -- This is the main one for the Outlook composer
  }

  -- Check the element and its parents (up to 5 levels)
  -- Sometimes the focus is on a paragraph inside the main text area.
  local current = element
  for i = 1, 5 do
    if not current then break end
    if textInputRoles[current:role()] then
      return true       -- We found a text input area
    end
    current = current:parent()
  end

  return false   -- No text input area was found
end

-- log = hs.logger.new('keycodes', 'info')
-- log.i('Initializing')


-- -- Refer to https://github.com/teddy-ma/dotfiles/blob/475c70d4c7dbc081fab7b7dc6ca59456a157281e/dotfiles.org#key

-- -- 3. Create the hotkey objects with conditional logic
-- local outlookRemaps = hs.eventtap.new(
--   {hs.eventtap.event.types.keyUp},
--   function(event)
--     local character = hs.keycodes.map[event:getKeyCode()]
--     log.i(character)
--     -- log.i(isTypingMode())
--     if isTypingMode() then
--       if character == '#' then
--         return hs.eventtap.keyString({'cmd'}, 'backspace')
--       elseif character == 'e' then
--         return hs.eventtap.keyStrong({'ctrl'}, 'e')
--       end
--     end
--   end
-- )

-- outlookRemaps:stop()

-- -- 4. Application watcher to enable/disable the hotkeys globally
-- -- This ensures the logic only runs when Outlook is the active app.
-- appWatcher = hs.application.watcher.new(function(appName, eventType, appObject)
--     if eventType == hs.application.watcher.activated then
--         if appObject:bundleID() == outlookBundleID then
--             outlookRemaps:start()
--             hs.alert.show("In Outlook")
--         else
--             outlookRemaps:stop()
--             -- hs.alert.show("Out Outlook")
--         end
--     end
-- end)

-- -- 5. Start the watcher
-- appWatcher:start()

-- -- Optional: Initial check for when the config is first loaded
-- local frontmostApp = hs.application.frontmostApplication()
-- if frontmostApp and frontmostApp:bundleID() == outlookBundleID then
--     outlookRemaps:start()
-- end

-- hs.alert.show("Context-Aware Outlook Config Loaded")
