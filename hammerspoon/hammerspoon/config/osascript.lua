local applications = require("config/applications")

local function launchViaOsascript(application, tabName)
    local tpl = [[
set windowIndex to 1

tell application "%s"
    repeat with theWindow in windows
        set tabIndex to 1
        repeat with theTab in tabs of theWindow
            if "%s" is in url of theTab then
                activate
                tell window windowIndex
                    set active tab index to tabIndex
                    set index to 1
                end tell
                tell application "System Events" to tell process "%s"
                    perform action "AXRaise" of window 1
                end tell
                return
            end
            set tabIndex to tabIndex + 1
        end repeat
        set windowIndex to windowIndex + 1
    end repeat
end tell

open location "%s"
    ]]

    local cmd = string.format(tpl, application, tabName, application, tabName)
    hs.osascript.applescript(cmd)
end

-- hs.hotkey.bind({'cmd', 'alt'}, 'm', function()
--     launchViaOsascript(applications.browser.name, "spotify.com")
-- end)
