require "config/applications"

-- constant holding the window enlargement/shrinkage factor
local FULLSCREEN_BORDER = 16
local ENABLE_FULLSCREEN_SHORTCUT = true
local ENABLE_FULLSCREEN_FOR_APPS = { applications.terminal }

-- Move window to the next screen
hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'o', function()
    local win = hs.window.focusedWindow()
    local nextScreen = win:screen():next()
    win:moveToScreen(nextScreen)
end)


function maximizeWindow()
    local app = hs.application.frontmostApplication()
    for _, allowed_app in ipairs(ENABLE_FULLSCREEN_FOR_APPS) do
        if app:title() ~= allowed_app then
            return
        end
    end

    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + FULLSCREEN_BORDER / 2
    f.y = max.y + FULLSCREEN_BORDER / 2
    f.w = max.w - FULLSCREEN_BORDER
    f.h = max.h - FULLSCREEN_BORDER
    win:setFrame(f)
end

if ENABLE_FULLSCREEN_SHORTCUT then
    hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'f', maximizeWindow)
end
