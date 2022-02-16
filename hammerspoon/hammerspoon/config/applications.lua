local applications = {
    terminal = {
        name = "iTerm",
        windowTitle = "iTerm2"
    },
    browser = {
        name = "Firefox",
    },
    email = {
        name = "Mail",
    },
    chat = {
        name = "Slack",
    },
    music = {
        name = "SomaFM",
    },
    documentation = {
        name = "Dash",
    },
    notes = {
        name = "Obsidian",
    },
    editor = {
        name = "Visual Studio Code",
        windowTitle = "Code",
    },
    video = {
        name = "Zoom Meeting",
    },
    youtube = {
        name = "YouTube",
    },
    todo = {
        name = "TickTick",
    },
}

-- overrides

for _, hostname in ipairs(hs.host.names()) do
    if string.find(hostname, "pixmac516") then
    end
end

return applications
