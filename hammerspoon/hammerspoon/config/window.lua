require "config/applications"

-- constant holding the window enlargement/shrinkage factor
local FULLSCREEN_BORDER = 16
local ENABLE_FULLSCREEN_SHORTCUT = true
local ENABLE_FULLSCREEN_FOR_APPS = {}
local WINDOW_BORDER = FULLSCREEN_BORDER
local LEFTRIGHT_FRACTION = 0.5
local TERMINAL_NORMAL_SIZE = {1024, 768}
local ENABLE_FULLSCREEN_FOR_APPS = { applications.terminal }

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
            if app:title() ~= allowed_app.name then
                return
            end
        end
    end

    local win = hs.window.focusedWindow()
    local f = win:frame()
    local prev_frame = {
        x = f.x,
        y = f.y,
        w = f.w,
        h = f.h,
    }
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + FULLSCREEN_BORDER / 2
    f.y = max.y + FULLSCREEN_BORDER / 2
    f.w = max.w - FULLSCREEN_BORDER
    f.h = max.h - FULLSCREEN_BORDER

    local found = false
    for _, application in pairs(applications) do
        if app:title() == application.name then
            if application.normal_size then
                toggleSize(win, prev_frame, f, application.normal_size)
                found = true
                break
            end
        end
    end

    if not found then
        win:setFrame(f)
    end
end

function toggleSize(win, current, target, default_size)
    local screen = win:screen()
    local max = screen:frame()
    if current.w == max.w - FULLSCREEN_BORDER and current.h == max.h - FULLSCREEN_BORDER then
        -- We are already fullscreened, so set the size down a bit
        target.w = default_size.width
        target.h = default_size.height
        -- center the window
        win:setFrame(target)
        win:centerOnScreen()
    else
        win:setFrame(target)
    end
end

if ENABLE_FULLSCREEN_SHORTCUT then
    hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'f', maximizeWindow)
end

-- Add the ability to add a zoom window at the top of the screen, or use the
-- remaining space for other windows
function zoomMode(target_height)
    return function()
        local win = hs.window.focusedWindow()
        local screen = win:screen()
        local max = screen:frame()
        local frame = win:frame()

        if win:title() == "Zoom Meeting" then
            frame.y = 0
            frame.h = target_height
        else
            frame.y = target_height
            frame.h = max.h - target_height
        end
        frame.x = 0
        frame.w = max.w

        win:setFrame(frame)
    end
end

hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'z', zoomMode(400))
