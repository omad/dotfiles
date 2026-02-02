
-- Making the Runtime, Funtime with Hammerspoon
-- https://blog.theodo.fr/2018/03/making-runtime-funtime-hammerspoon/
--
-- Just Enough Lua to Be Productive in Hammerspoon, Part 1
-- http://zzamboni.org/post/just-enough-lua-to-be-productive-in-hammerspoon-part-1/
--
-- My Hammerspoon configuration with commentary
-- http://zzamboni.org/post/my-hammerspoon-configuration-with-commentary/

local hyper = {"cmd", "ctrl"}


-----------------------------------------------
-- Reload config on write
-----------------------------------------------

hs.loadSpoon("ReloadConfiguration")
spoon.ReloadConfiguration:start()
-- hs.alert.show("Config loaded")
hs.notify.new({title="Hammerspoon", informativeText="Config loaded"}):send()

-- hs.loadSpoon("SpoonInstall")
-- -- hs.loadSpoon("WindowHalfsAndThirds")
-- hs.loadSpoon("WindowScreenLeftAndRight")

-- spoon.WindowScreenLeftAndRight:bindHotkeys({
--     screen_left = { hyper, "Left" },
--     screen_right = { hyper, "Right" }
-- })

-- Enable IPC for using the `hs` CLI
require("hs.ipc")

-- See https://github.com/AaronLasseigne/dotfiles/blob/50d2325c1ad7552ea95a313fbf022004e2932ce9/.hammerspoon/init.lua
-- on branch 'master' it has been significantly updated
hs.window.animationDuration = 0

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "W", function()
    -- hs.alert.show("Hello World!")
    hs.notify.new({title="Hammerspoon", informativeText="Hello World"}):send()
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
function toggle_window_maximized()
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

positions = {
  maximized = hs.layout.maximized,
  centered = {x=0.15, y=0.15, w=0.7, h=0.7},

  left34 = {x=0, y=0, w=0.34, h=1},
  left50 = hs.layout.left50,
  left66 = {x=0, y=0, w=0.66, h=1},

  right34 = {x=0.66, y=0, w=0.34, h=1},
  right50 = hs.layout.right50,
  right66 = {x=0.34, y=0, w=0.66, h=1},

  upper50 = {x=0, y=0, w=1, h=0.5},
  upper50Left50 = {x=0, y=0, w=0.5, h=0.5},
  upper50Right50 = {x=0.5, y=0, w=0.5, h=0.5},

  lower50 = {x=0, y=0.5, w=1, h=0.5},
  lower50Left50 = {x=0, y=0.5, w=0.5, h=0.5},
  lower50Right50 = {x=0.5, y=0.5, w=0.5, h=0.5}
}

-- hs.hotkey.bind(hyper, "k", function()
--                  local win = hs.window.focusedWindow()
--                  win:setFrame(win:screen():frame())

-- end)

function bindKey(key, fn)
  hs.hotkey.bind(hyper, key, fn)
end

grid = {
  {key="u", units={positions.upper50Left50}},
  {key="i", units={positions.upper50}},
  {key="o", units={positions.upper50Right50}},

  {key="j", units={positions.left50, positions.left66, positions.left34}},
  -- {key="k", units={positions.centered, positions.maximized}},
  {key="l", units={positions.right50, positions.right66, positions.right34}},

  {key="m", units={positions.lower50Left50}},
  -- Comment out comma, it conflicts with reload kitty config
  -- {key=",", units={positions.lower50}},
  {key=".", units={positions.lower50Right50}}
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
local downloadLog = hs.logger.new('DLMonitor','debug')

local trash = os.getenv("HOME") .. "/.Trash"
local downloadFolder = os.getenv("HOME") .. "/Downloads/"

local function moveToTrash(path)
	-- local _displayName = hs.fs.displayName(path)
	-- os.rename(path, trash .. _displayName)
	hs.task.new("/usr/bin/trash", function(exitCode, stdOut, stdErr)
	  if exitCode == 0 then
	    downloadLog.i("Success moving ", path, " to the Trash.")
    else
      downloadLog.e("Error moving ", path, " to the Trash.", stdOut, stdErr)
    end
	end, {"--stopOnError", "--verbose", path}):start()
end

function handle_download(paths, flagstable)
  -- paths: a table containing a list of file paths that have changed
  -- flagTables: a table containing a list of tables denoting how each corresponding file in paths has changed,
  -- each containing boolean values indicating which types of events occurred;
  -- The possible keys are:
  --  * mustScanSubDirs
  --  * userDropped
  --  * kernelDropped
  --  * eventIdsWrapped
  --  * historyDone
  --  * rootChanged
  --  * mount
  --  * unmount
  --  * itemCreated
  --  * itemRemoved
  --  * itemInodeMetaMod
  --  * itemRenamed
  --  * itemModified
  --  * itemFinderInfoMod
  --  * itemChangeOwner
  --  * itemXattrMod
  --  * itemIsFile
  --  * itemIsDir
  --  * itemIsSymlink
  --  * ownEvent (OS X 10.9+)
  --  * itemIsHardlink (OS X 10.10+)
  --  * itemIsLastHardlink (OS X 10.10+)
  -- uhm
  local transmissionRemote = "/opt/homebrew/bin/transmission-remote"

   for file in hs.fs.dir(os.getenv("HOME") .. "/Downloads/") do
     local fullPath = downloadFolder .. file
     if file:match("%.torrent$") ~= nil then
       hs.task.new(transmissionRemote, function(exitCode, stdOut, stdErr)
         if exitCode == 0 then
            local msg = "[torrent-watch] added + trashed: " .. file
            --hs.notify.new({title="Hammerspoon Download Monitor", informativeText=msg}):send()
            hs.notify.show("HS Download Monitor", "Success sending to Transmission", msg)
            print(msg)
            moveToTrash(fullPath)
          else
            local msg = string.format("[torrent-watch] FAILED (%d): %s\nstdout: %s\nstderr: %s", exitCode, file, stdOut or "", stdErr or "")

            --hs.notify.new({title="Download Monitor", informativeText="Failed to send " .. file})
            hs.notify.show("HS Download Monitor", "Failed sending to Transmission", msg)
            print(msg)
          end
        end, {"nixos", "--add", fullPath}):start()
     end
   end

--   local hello = "hello"
--   hs.notify.new({title="Hammerspoon Download Monitor", informativeText="changed!"}):send()
--   for k,v in pairs(paths) do
--     hs.notify.new({title="Hammerspoon Download Monitor", informativeText=k .. ": " .. v}):send()
--     downloadLog.i("Altered " .. k .. " " .. v)
--   end
--   for k,v in pairs(flagstable) do
-- --    hs.notify.new({title="Hammerspoon Download Monitor", informativeText=k .. ": " .. v}):send()
--     downloadLog.i("Foo " .. k .. #v) -- .. ": " .. v)
--     for k2,v2 in pairs(v) do
--       downloadLog.i("Bar " .. k2) -- .. v2) -- .. ": " .. v)
--     end
--   end
end

local downloadWatcher = hs.pathwatcher.new(downloadFolder, handle_download)
downloadWatcher:start()



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
outlookBundleID = "com.microsoft.Outlook"

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
        ["AXWebArea"] = true, -- This is the main one for the Outlook composer
    }

    -- Check the element and its parents (up to 5 levels)
    -- Sometimes the focus is on a paragraph inside the main text area.
    local current = element
    for i = 1, 5 do
        if not current then break end
        if textInputRoles[current:role()] then
            return true -- We found a text input area
        end
        current = current:parent()
    end

    return false -- No text input area was found
end

log = hs.logger.new('keycodes', 'info')
log.i('Initializing')


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
