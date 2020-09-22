-- constant holding the window enlargement/shrinkage factor
local FULLSCREEN_BORDER = 16
local ENABLE_FULLSCREEN_SHORTCUT = true
local WINDOW_BORDER = FULLSCREEN_BORDER
local LEFTRIGHT_FRACTION = 0.5
local TERMINAL_NORMAL_SIZE = {1024, 768}
local ENABLE_FULLSCREEN_FOR_APPS = {}


fc = FrameCache:new()

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

    fc:add(win)

    f.x = max.x
    f.y = max.y
    f.w = halfWindowWidth(max)
    f.h = max.h
    f = clampFrame(f, max)
    win:setFrame(f)
end)

function sign(value)
    return value / math.abs(value)
end

-- Move window to the right half
hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'Right', function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    fc:add(win)

    f.x = sign(f.x) * (max.w / 2 + FULLSCREEN_BORDER * 1 / 4)
    f.y = max.y
    f.w = halfWindowWidth(max)
    f.h = max.h
    f = clampFrame(f, max)
    win:setFrame(f)
end)

function halfWindowWidth(max)
    return max.w / 2 - FULLSCREEN_BORDER * 3 / 4
end


function maximizeWindow()
    local app = hs.application.frontmostApplication()
    if #ENABLE_FULLSCREEN_FOR_APPS ~= 0 then
        local found = false
        for _, allowed_app in ipairs(ENABLE_FULLSCREEN_FOR_APPS) do
            if string.lower(app:title()) == string.lower(allowed_app.name) then
                found = true
                break
            end
        end
        if not found then
            return
        end
    end

    local win = hs.window.focusedWindow()
    local f = win:frame()

    local screen = win:screen()
    local max = screen:frame()

    fc:add(win)

    f.x = max.x
    f.y = max.y
    f.w = max.w
    f.h = max.h
    f = clampFrame(f, max)

    win:setFrame(f)
end

function clamp(val, minval, maxval)
    return math.min(math.max(val, minval), maxval)
end

function clampFrame(frame, max)
    frame.x = clamp(frame.x, max.x + FULLSCREEN_BORDER / 2, max.w - FULLSCREEN_BORDER / 2 + max.x)
    frame.y = clamp(frame.y, max.y + FULLSCREEN_BORDER / 2, max.h - FULLSCREEN_BORDER / 2 + max.y)
    frame.w = clamp(frame.w, 0, max.w - FULLSCREEN_BORDER - math.max(0, max.x))
    frame.h = clamp(frame.h, 0, max.h - FULLSCREEN_BORDER)
    return frame
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
            frame.y = target_height + max.y + FULLSCREEN_BORDER * 2.5
            frame.h = max.h + max.y - target_height - FULLSCREEN_BORDER * 2
        end
        frame.x = 0
        frame.w = max.w

        frame = clampFrame(frame, max)

        win:setFrame(frame)
    end
end

hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'z', function()
    local win = hs.window.focusedWindow()
    local app = win:application()
    local name = app:name()

    fc:pop(name)
end)
