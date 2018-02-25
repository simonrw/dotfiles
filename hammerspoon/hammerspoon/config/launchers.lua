-- Terminal
hs.hotkey.bind({'cmd', 'alt'}, 't', function()
    hs.application.launchOrFocus('kitty')
end)

-- Browser
hs.hotkey.bind({'cmd', 'alt'}, 'c', function()
    hs.application.launchOrFocus('Safari')
end)

hs.hotkey.bind({'cmd', 'alt'}, 'e', function()
    hs.application.launchOrFocus('Mail')
end)
