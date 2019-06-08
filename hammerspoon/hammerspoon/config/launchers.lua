terminal = "Kitty"
browser = "Firefox Nightly"
email = "Mail"

hs.hotkey.bind({'cmd', 'alt'}, 'c', function()
    hs.application.launchOrFocus(browser)
end)

hs.hotkey.bind({'cmd', 'alt'}, 't', function()
    hs.application.launchOrFocus(terminal)
end)

hs.hotkey.bind({'cmd', 'alt'}, 'e', function()
    hs.application.launchOrFocus(email)
end)
