local applications = require('config/applications')

-- helper function to bind multiple keys to a single application
local seen_hotkeys = {}
local function bindKey(application, ...)
    local arg = { ... }
    for _, k in ipairs(arg) do
        -- check if the key has been bound already
        if seen_hotkeys[k] then
            hs.alert.show('Key already bound')
            return
        end

        hs.hotkey.bind({ 'cmd', 'alt' }, k, function()
            hs.application.launchOrFocus(application.name)
        end)

        -- add the hotkey to the seen list
        seen_hotkeys[k] = true
    end
end

bindKey(applications.browser, 'c')
bindKey(applications.terminal, 't')
bindKey(applications.chat, 's')
bindKey(applications.linear, 'l')
bindKey(applications.todo, 'r')
bindKey(applications.notes, 'e')
