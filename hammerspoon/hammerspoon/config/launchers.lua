terminalEmulator = "iTerm"
browser = "Firefox Developer Edition"

function launchTerminal()
    hs.application.launchOrFocus(terminalEmulator)
end

function launchEditor()
    hs.application.launchOrFocus('Visual Studio Code')
end

function launchEmail()
    hs.application.launchOrFocus('Mail')
end

function launchTerminal()
    hs.application.launchOrFocus(terminalEmulator)
end

-- Browser
hs.hotkey.bind({'cmd', 'alt'}, 'c', function()
    hs.application.launchOrFocus(browser)
end)

hs.hotkey.bind({'cmd', 'alt'}, 't', launchTerminal)

-- Editor
-- hs.hotkey.bind({'cmd', 'alt'}, 'e', launchEmail)

-- Switch between editor and terminal
hs.hotkey.bind({'cmd', 'alt'}, 's', function()
    local currentApplication = hs.application.frontmostApplication()
    local currentApplicationName = currentApplication:name():lower()
    if currentApplicationName == 'code' then
        hs.application.launchOrFocus(terminalEmulator)
    elseif currentApplicationName == terminalEmulator then
        hs.application.launchOrFocus('Visual Studio Code')
    end
end)
