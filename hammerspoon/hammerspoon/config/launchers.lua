-- Terminal
hs.hotkey.bind({'cmd', 'alt'}, 't', function()
    hs.application.launchOrFocus('iTerm')
end)

-- Browser
hs.hotkey.bind({'cmd', 'alt'}, 'c', function()
    hs.application.launchOrFocus('Google Chrome')
end)

-- Email
hs.hotkey.bind({'cmd', 'alt'}, 'm', function()
    hs.application.launchOrFocus('Mail')
end)

-- Atom
hs.hotkey.bind({'cmd', 'alt'}, 'e', function()
    hs.application.launchOrFocus('Atom')
end)

hs.hotkey.bind({'cmd', 'alt'}, 'n', function()
    hs.application.launchOrFocus('Notational Velocity')
end)


-- Hammerspoon window
hs.hotkey.bind({'cmd', 'shift', 'alt'}, 'h', function()
    hs.application.launchOrFocus('Hammerspoon')
end)
