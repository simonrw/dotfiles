terminal = "Kitty"
browser = "Google Chrome"
email = "Mail"

hs.hotkey.bind({'cmd', 'alt'}, 'c', function()
    hs.application.launchOrFocus(browser)
end)

hs.hotkey.bind({'cmd', 'alt'}, 't', function()
    hs.application.launchOrFocus(terminal)
end)
