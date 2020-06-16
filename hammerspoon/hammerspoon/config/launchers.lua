require "config/applications"

-- helper function to bind multiple keys to a single application
function bindKey(application, ...)
    local arg = {...}
    for _, k in ipairs(arg) do
        hs.hotkey.bind({'cmd', 'alt'}, k, function()
            hs.application.launchOrFocus(application)
        end)
    end
end

bindKey(applications.browser, 'c')
bindKey(applications.music, 'm')
bindKey(applications.chat, 's')
bindKey(applications.terminal, 't')
bindKey(applications.documentation, 'r')
bindKey(applications.notes, 'n')
bindKey(applications.email, 'e')
