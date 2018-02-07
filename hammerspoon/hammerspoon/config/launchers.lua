-- Terminal
hs.hotkey.bind({'cmd', 'alt'}, 't', function()
    hs.application.launchOrFocus('kitty')
end)

-- Browser
hs.hotkey.bind({'cmd', 'alt'}, 'c', function()
    hs.application.launchOrFocus('Google Chrome')
end)

-- Notational velocity
hs.hotkey.bind({'cmd', 'alt'}, 'n', function()
    hs.application.launchOrFocus('Notational Velocity')
end)

hs.hotkey.bind({'cmd', 'alt'}, 'e', function()
    hs.application.launchOrFocus('Mail')
end)

hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'i', function()
    hs.application.launchOrFocus('LimeChat')
end)

-- Hammerspoon window
hs.hotkey.bind({'cmd', 'shift', 'alt'}, 'h', function()
    hs.application.launchOrFocus('Hammerspoon')
end)
