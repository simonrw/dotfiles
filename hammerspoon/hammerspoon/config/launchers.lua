terminal = "Alacritty"
browser = "Firefox"
email = "Mail"
chat = "Slack"
music = "Spotify"
documentation = "Dash"
notes = "Notable"

hs.hotkey.bind({'cmd', 'alt'}, 'c', function()
    hs.application.launchOrFocus(browser)
end)

hs.hotkey.bind({'cmd', 'alt'}, 'm', function()
    hs.application.launchOrFocus(music)
end)

hs.hotkey.bind({'cmd', 'alt'}, 's', function()
    hs.application.launchOrFocus(chat)
end)

hs.hotkey.bind({'cmd', 'alt'}, 't', function()
    hs.application.launchOrFocus(terminal)
end)

hs.hotkey.bind({'cmd', 'alt'}, 'r', function()
    hs.application.launchOrFocus(documentation)
end)

hs.hotkey.bind({'cmd', 'alt'}, 'n', function()
    hs.application.launchOrFocus(notes)
end)
