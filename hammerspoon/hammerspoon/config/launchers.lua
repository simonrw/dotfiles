-- Terminal
hs.hotkey.bind({'cmd', 'alt'}, 't', function()
    hs.application.launchOrFocus('Kitty')
end)

-- Browser
hs.hotkey.bind({'cmd', 'alt'}, 'c', function()
    hs.application.launchOrFocus('Firefox')
end)
