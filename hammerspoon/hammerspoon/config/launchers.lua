local applications = require("config/applications")

-- helper function to bind multiple keys to a single application
local seen_hotkeys = {}
function bindKey(application, ...)
    local arg = {...}
    for _, k in ipairs(arg) do
        -- check if the key has been bound already
        if seen_hotkeys[k] then
            hs.alert.show('Key already bound')
            return
        end

        hs.hotkey.bind({'cmd', 'alt'}, k, function()
            hs.application.launchOrFocus(application.name)
        end)

        -- add the hotkey to the seen list
        seen_hotkeys[k] = true
    end
end

bindKey(applications.browser, 'c')
bindKey(applications.terminal, 't')
bindKey(applications.editor, 'y')
bindKey(applications.documentation, 'r')
bindKey(applications.notes, 'e')
bindKey(applications.todo, 'n')
bindKey(applications.music, 'm')
