local applications = require("config/applications")

-- helper function to bind multiple keys to a single application
local seen_hotkeys = {}
local function bindKey(application, ...)
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
bindKey(applications.documentation, 'r')
bindKey(applications.notes, 'e')
bindKey(applications.todo, 'n')
bindKey(applications.music, 'm')
bindKey(applications.editor, 'y')
bindKey(applications.chat, 's')

local BundleCache = {}
BundleCache.__index = BundleCache

function BundleCache:new()
    local props = {}
    setmetatable(props, BundleCache)
    props.bundles = {}
    return props
end

function BundleCache:get(name)
    if self.bundles[name] == nil then
        local app = hs.application.get(name)
        if app then
            self.bundles[name] = app:bundleID()
        end
    end

    local bundleId = self.bundles[name]
    if bundleId then
        return bundleId
    else
        hs.alert.show("Could not find bundle for application: " .. name)
    end
end

local bc = BundleCache:new()

-- set default browser
local function handleBrowse(scheme, host, params, fullURL, senderPID)
    local bundleId = bc:get(applications.browser.name)
    hs.urlevent.openURLWithBundle(fullURL, bundleId)
end

hs.application.enableSpotlightForNameSearches(true)
hs.urlevent.httpCallback = handleBrowse
