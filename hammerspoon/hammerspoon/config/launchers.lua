terminal = "Alacritty"
browser = "Firefox"
email = "Mail"
chat = "Slack"
music = "Spotify"
documentation = "Dash"
notes = "Notable"


-- helper function to bind multiple keys to a single application
function bindKey(application, ...)
    local arg = {...}
    for _, k in ipairs(arg) do
        hs.hotkey.bind({'cmd', 'alt'}, k, function()
            hs.application.launchOrFocus(application)
        end)
    end
end

bindKey(browser, 'c')
bindKey(music, 'm')
bindKey(chat, 's')
bindKey(terminal, 't')
bindKey(documentation, 'r')
bindKey(notes, 'n')
bindKey(email, 'e')
