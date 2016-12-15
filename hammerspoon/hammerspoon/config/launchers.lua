-- Terminal
hs.hotkey.bind({'cmd', 'alt'}, 't', function()
    hs.application.launchOrFocus('iTerm')
end)

-- Browser
hs.hotkey.bind({'cmd', 'alt'}, 'c', function()
    hs.application.launchOrFocus('Google Chrome')
end)

-- Hammerspoon window
hs.hotkey.bind({'cmd', 'alt'}, 'h', function()
    hs.application.launchOrFocus('Hammerspoon')
end)

