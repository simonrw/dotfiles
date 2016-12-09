-- Disable all window animations when changing
hs.window.animationDuration = 0


-- Application launchers

-- Terminal
hs.hotkey.bind({'cmd', 'alt'}, 't', function()
    hs.application.launchOrFocus('iTerm')
end)

-- Browser
hs.hotkey.bind({'cmd', 'alt'}, 'c', function()
    hs.application.launchOrFocus('Google Chrome')
end)

-- Window manipulation

-- constant holding the window enlargement/shrinkage factor
WINDOW_SIZE_CHANGE = 20

-- Move window to the next screen
hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'o', function()
    local win = hs.window.focusedWindow()
    local nextScreen = win:screen():next()
    win:moveToScreen(nextScreen)
end)

-- Center window
hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'c', function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local screenFrame = screen:frame()

    f.x = (screenFrame.w / 2) - (f.w / 2)
    f.y = (screenFrame.h / 2) - (f.h / 2)
    f.w = f.w
    f.h = f.h
    win:setFrame(f)
end)

-- Move window to left half
hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'Left', function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + WINDOW_SIZE_CHANGE / 2
    f.y = max.y + WINDOW_SIZE_CHANGE / 2
    f.w = max.w / 2 - WINDOW_SIZE_CHANGE / 2

    f.h = max.h - WINDOW_SIZE_CHANGE
    win:setFrame(f)
end)

-- Move window to the right half
hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'Right', function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + (max.w / 2)
    f.y = max.y + WINDOW_SIZE_CHANGE / 2
    f.w = max.w / 2 - WINDOW_SIZE_CHANGE / 2
    f.h = max.h - WINDOW_SIZE_CHANGE
    win:setFrame(f)
end)

-- Maximise window
hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'f', function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + WINDOW_SIZE_CHANGE / 2
    f.y = max.y + WINDOW_SIZE_CHANGE / 2
    f.w = max.w - WINDOW_SIZE_CHANGE
    f.h = max.h - WINDOW_SIZE_CHANGE
    win:setFrame(f)
end)

-- Make window smaller
hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, '-', function()
    local win = hs.window.focusedWindow()
    local f = win:frame()

    f.w = f.w - WINDOW_SIZE_CHANGE
    f.h = f.h - WINDOW_SIZE_CHANGE
    f.x = f.x + WINDOW_SIZE_CHANGE / 2
    f.y = f.y + WINDOW_SIZE_CHANGE / 2
    win:setFrame(f)
end)

-- Make window larger
hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, '=', function()
    local win = hs.window.focusedWindow()
    local f = win:frame()

    f.w = f.w + WINDOW_SIZE_CHANGE
    f.h = f.h + WINDOW_SIZE_CHANGE
    f.x = f.x - WINDOW_SIZE_CHANGE / 2
    f.y = f.y - WINDOW_SIZE_CHANGE / 2
    win:setFrame(f)
end)

-- Switch focused
-- Set the hint style
hs.hints.style = 'vimperator'
hs.hotkey.bind({'cmd', 'shift'}, 'Space', function()
    hs.hints.windowHints()
end)

-- Change brightnesses
current_brightness = hs.screen.mainScreen():getBrightness()
brightness_change = 0.2
function handleWindowChange(name, notify_type, application)
    if notify_type == hs.application.watcher.activated then
        if name == 'iTerm2' then
            current_brightness = hs.screen.mainScreen():getBrightness()
            local new_brightness = math.max(math.min(current_brightness + brightness_change, 1.0), 0.0)
            hs.screen.mainScreen():setBrightness(new_brightness)
        else
            hs.screen.mainScreen():setBrightness(current_brightness)
        end
    end
end

watcher = hs.application.watcher.new(handleWindowChange)
watcher:start()

-- Reload the config on file change
hs.pathwatcher.new(os.getenv('HOME') .. '/.hammerspoon/', function(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == '.lua' then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end):start()

hs.alert.show('Config reloaded')
