require "config/applications"

-- constant holding the window enlargement/shrinkage factor
local FULLSCREEN_BORDER = 16
local ENABLE_FULLSCREEN_SHORTCUT = true
local ENABLE_FULLSCREEN_FOR_APPS = {}
local WINDOW_BORDER = FULLSCREEN_BORDER
local LEFTRIGHT_FRACTION = 0.5

-- Move window to the next screen
hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'o', function()
    local win = hs.window.focusedWindow()
    local nextScreen = win:screen():next()
    win:moveToScreen(nextScreen)
end)

-- Move window to left two thirds
hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'Left', function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + WINDOW_BORDER / 2
    f.y = max.y + WINDOW_BORDER / 2
    f.w = LEFTRIGHT_FRACTION * max.w - WINDOW_BORDER
    f.h = max.h - WINDOW_BORDER
    win:setFrame(f)
end)

-- Move window to the right half
hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'Right', function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + LEFTRIGHT_FRACTION * max.w
    f.y = max.y + WINDOW_BORDER / 2
    f.w = (1 - LEFTRIGHT_FRACTION) * max.w - WINDOW_BORDER / 2.0
    f.h = max.h - WINDOW_BORDER
    win:setFrame(f)
end)


function maximizeWindow()
    local app = hs.application.frontmostApplication()
    if #ENABLE_FULLSCREEN_FOR_APPS ~= 0 then
        for _, allowed_app in ipairs(ENABLE_FULLSCREEN_FOR_APPS) do
            if app:title() ~= allowed_app then
                return
            end
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
