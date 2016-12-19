-- constant holding the window enlargement/shrinkage factor
local WINDOW_SIZE_CHANGE = 16
local WINDOW_BORDER = 16

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

    f.x = max.x + WINDOW_BORDER / 2
    f.y = max.y + WINDOW_BORDER / 2
    f.w = max.w / 2 - WINDOW_BORDER / 2
    f.h = max.h - WINDOW_BORDER
    win:setFrame(f)
end)

-- Move window to the right half
hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'Right', function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + (max.w / 2)
    f.y = max.y + WINDOW_BORDER / 2
    f.w = max.w / 2 - WINDOW_BORDER / 2
    f.h = max.h - WINDOW_BORDER
    win:setFrame(f)
end)

function maximizeWindow()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + WINDOW_BORDER / 2
    f.y = max.y + WINDOW_BORDER / 2
    f.w = max.w - WINDOW_BORDER
    f.h = max.h - WINDOW_BORDER
    win:setFrame(f)
end

-- Maximise window
hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'f', maximizeWindow)

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


