-- Terminal
hs.hotkey.bind({'cmd', 'alt'}, 't', function()
    hs.application.launchOrFocus('iTerm')
end)

-- Browser
hs.hotkey.bind({'cmd', 'alt'}, 'c', function()
    hs.application.launchOrFocus('FirefoxDeveloperEdition')
end)

-- Email
hs.hotkey.bind({'cmd', 'alt'}, 'e', function()
    hs.application.launchOrFocus('Mail')
end)

-- Hammerspoon window
hs.hotkey.bind({'cmd', 'shift', 'alt'}, 'h', function()
    hs.application.launchOrFocus('Hammerspoon')
end)

