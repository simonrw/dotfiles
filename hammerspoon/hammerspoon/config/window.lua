-- constant holding the window enlargement/shrinkage factor
local WINDOW_SIZE_CHANGE = 16
local WINDOW_BORDER = 50
local FULLSCREEN_BORDER = WINDOW_BORDER
local LEFTRIGHT_FRACTION = 0.55

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

    f.x = WINDOW_BORDER / 2
    f.y = max.y + WINDOW_BORDER / 2
    f.w = LEFTRIGHT_FRACTION * max.w - WINDOW_BORDER / 2.0
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
