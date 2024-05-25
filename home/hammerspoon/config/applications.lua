local applications = {
    terminal = {
        name = "Wezterm",
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
        name = "Spotify",
    },
    documentation = {
        name = "Dash",
    },
    notes = {
        name = "Notion",
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
