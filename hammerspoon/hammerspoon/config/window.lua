-- constant holding the window enlargement/shrinkage factor
local applications = require('config/applications')
local FULLSCREEN_BORDER = 0
local ENABLE_FULLSCREEN_SHORTCUT = false
local ENABLE_SIDE_WINDOW_SHORTCUTS = true
local WINDOW_BORDER = FULLSCREEN_BORDER
local LEFTRIGHT_FRACTION = 0.5
local TERMINAL_NORMAL_SIZE = {1024, 768}
local ENABLE_FULLSCREEN_FOR_APPS = {}
local MOVE_AMOUNT = 20


fc = FrameCache:new()

function windowLeftHalf(frame, max, border)
    local border = border or WINDOW_BORDER

    local newFrame = {
        x = max.x + border / 2,
        y = max.y + border / 2,
        w = halfWindowWidth(max, border),
        h = max.h - border,
    }

    return newFrame
end

function windowRightHalf(frame, max, border)
    local border = border or WINDOW_BORDER

    local newFrame = {
        x = sign(max.x) * (max.w / 2) + border / 4,
        y = max.y + border / 2,
        w = halfWindowWidth(max, border),
        h = max.h - border,
    }

    return newFrame
end


function sign(value)
    if value == 0 then
        return 1
    end

    return value / math.abs(value)
end

function halfWindowWidth(max, border)
    border = border or WINDOW_BORDER
    return max.w / 2 - border * 3 / 4
end


function maximizeWindowSize(frame, max, border)
    local border = border or FULLSCREEN_BORDER

    return clampFrame(max, max, border)
end

function clamp(val, minval, maxval)
    return math.min(math.max(val, minval), maxval)
end

function clampFrame(frame, max, fullscreen_border)
    local fullscreen_border = fullscreen_border or FULLSCREEN_BORDER

    local newFrame = {
        x = clamp(frame.x, max.x + fullscreen_border / 2, max.w - fullscreen_border / 2 + max.x),
        y = clamp(frame.y, max.y + fullscreen_border / 2, max.h - fullscreen_border / 2 + max.y),
        w = clamp(frame.w, 0, max.w - fullscreen_border - math.max(0, max.x)),
        h = clamp(frame.h, 0, max.h - fullscreen_border),
    }
    return newFrame
end

-- HANDLERS
-- Move window to the next screen
hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'o', function()
    local win = hs.window.focusedWindow()
    local nextScreen = win:screen():next()
    fc:add(win)
    win:moveToScreen(nextScreen)
end)

if ENABLE_SIDE_WINDOW_SHORTCUTS then
    -- Move window to left two thirds
    hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'Left', function()
        local win = hs.window.focusedWindow()
        local f = win:frame()
        local screen = win:screen()
        local max = screen:frame()

        fc:add(win)

        local f = windowLeftHalf(f, max)

        win:setFrame(f)
    end)

    -- Move window to the right half
    hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'Right', function()
        local win = hs.window.focusedWindow()
        local f = win:frame()
        local screen = win:screen()
        local max = screen:frame()

        fc:add(win)

        local f = windowRightHalf(f, max)

        win:setFrame(f)
    end)
end

hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'z', function()
    local win = hs.window.focusedWindow()
    local app = win:application()
    local name = app:name()

    fc:pop(name)
end)

local LEFT = {-MOVE_AMOUNT, 0}
local RIGHT = {MOVE_AMOUNT, 0}
local UP = {0, -MOVE_AMOUNT}
local DOWN = {0, MOVE_AMOUNT}

function setupMoveKey(key, direction)
    local timer = nil

    function pressedCallback()
        local win = hs.window.focusedWindow()
        fc:add(win)
        win:move(direction, nil, true)
        timer = hs.timer.doEvery(0.01, function()
            win:move(direction, nil, true)
        end)
    end

    function releasedCallback()
        if timer ~= nil then
            timer:stop()
            timer = nil
        end
    end

    hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, key, pressedCallback, releasedCallback)
end

setupMoveKey("j", DOWN)
setupMoveKey('k', UP)
setupMoveKey('h', LEFT)
setupMoveKey('l', RIGHT)

if ENABLE_FULLSCREEN_SHORTCUT then
    hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'f', function()
        local app = hs.application.frontmostApplication()
        if #ENABLE_FULLSCREEN_FOR_APPS ~= 0 then
            local found = false
            for _, allowed_app in ipairs(ENABLE_FULLSCREEN_FOR_APPS) do
                local windowTitle = string.lower(allowed_app.windowTitle or allowed_app.name)
                if string.lower(app:title()) == windowTitle then
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

        local f = maximizeWindowSize(f, max)

        win:setFrame(f)
    end)

    -- maximise over multiple screens
    hs.hotkey.bind({'cmd', 'alt', 'ctrl', 'shift'}, 'f', function()
        print("Really maximising this window")

        local allScreens = hs.screen.allScreens()
        if #allScreens > 1 then
            local win = hs.window.focusedWindow()

            local r = hs.geometry(0, 0, 0, 0)

            for _, screen in ipairs(allScreens) do
                local f = screen:frame()
                print(hs.inspect(f))
                r.x = math.min(r.x, f.x)
                r.y = math.max(r.y, f.y)
                r.w = r.w + f.w
                if r.h == 0 then
                    r.h = f.h
                else
                    r.h = math.min(r.h, f.h)
                end
            end

            print(hs.inspect(r))

            fc:add(win)
            win:setFrame(r)
        end
    end)
end

