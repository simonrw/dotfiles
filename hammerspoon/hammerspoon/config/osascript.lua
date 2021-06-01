require "config/applications"

function switchTo(url, browserName)
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

    local script = string.format(tpl, browserName, url, browserName, url)
    hs.osascript.applescript(script)
end

-- Slack shortcut
hs.hotkey.bind({'cmd', 'alt'}, 's', function()
    switchTo("slack.com", applications.browser.name)
end)
