local applications = require('config/applications')

-- set default browser
local function handleBrowse(scheme, host, params, fullURL, senderPID)
    local browser_name = applications.browser.name
    hs.application.launchOrFocus(browser_name)
    local app = hs.application.get(browser_name)
    if app == nil then
        hs.application.launchOrFocus(browser_name)
        app = hs.application.get(browser_name)
    end
    local bundle_id = app:bundleID()

    hs.urlevent.openURLWithBundle(fullURL, bundle_id)
end

hs.application.enableSpotlightForNameSearches(true)
hs.urlevent.httpCallback = handleBrowse
