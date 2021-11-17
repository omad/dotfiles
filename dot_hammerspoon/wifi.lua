
-- wifiWatcher = nil
-- workSSID = "GA Staff"
-- lastSSID = hs.wifi.currentNetwork()

-- netConf = hs.network.configuration.open()

-- hs.alert.show("Wifi Config Loading")

-- function ssidChangedCallback(watcher, message, interface)
--   hs.alert.show("ssidChangedCallback" .. watcher .. message .. interface)
--   local newSSID = hs.wifi.currentNetwork()
--   if newSSID then
--     hs.alert.show("Network connected: " .. ssid)
--   end

--   -- and lastSSID ~= workSSID
--   if newSSID == workSSID then
--     -- We just joined our work WiFi network
--     -- hs.audiodevice.defaultOutputDevice():setVolume(25)
-- --    app = hs.appfinder.appFromName("Transmission")
-- --    app:kill()

--     netConf:setLocation("Work")

--     hs.application.launchOrFocus("Skype for Business")
--   elseif newSSID ~= workSSID and lastSSID == workSSID then
--     -- We just departed our work WiFi network
--     -- hs.audiodevice.defaultOutputDevice():setVolume(0)
--   else
--     netConf:setLocation("Automatic")

--   end

--   lastSSID = newSSID
-- end

-- wifiWatcher = hs.wifi.watcher.new(ssidChangedCallback)
-- wifiWatcher:start()
Install = spoon.SpoonInstall

connectedToWork = function(_, _, prev_ssid, new_ssid)
    hs.execute('networksetup -setproxyautodiscovery Wi-Fi on')
    app = hs.appfinder.appFromName("Transmission")
    app:kill()
end


Install:andUse("WiFiTransitions",
   {
     config = {
       actions = {
         { -- Test action just to see the SSID transitions
            fn = function(_, _, prev_ssid, new_ssid)
               hs.notify.show("SSID change", string.format("From '%s' to '%s'", prev_ssid, new_ssid), "")
            end
         },
         {
             to = "Joplin",
             fn = connectedToWork
         },
         { -- Enable proxy at work
             to = "GA Staff",
             fn = connectedToWork
           -- fn = {hs.fnutils.partial(reconfigSpotifyProxy, true),
           --       hs.fnutils.partial(reconfigAdiumProxy, true),
           -- }
         },
         {
           to = "ArapsoNet",
           fn = function(_, _, prev_ssid, new_ssid)
             hs.execute('networksetup -setproxyautodiscovery Wi-Fi off')
--              hs.application.open("Transmission")
           end
         },
         { -- Disable proxy in Spotify and Adium config when leaving corp network
           from = "GA Staff",
           fn = function(_, _, prev_ssid, new_ssid)
             hs.execute('networksetup -setproxyautodiscovery Wi-Fi off')
           end
         },
       }
     },
     start = true,
   }
)
