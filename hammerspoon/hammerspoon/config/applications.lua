applications = {
    terminal = {
        name = "Alacritty",
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
        name = "Obsidian",
    },
    editor = {
        name = "Visual Studio Code",
    },
    video = {
        name = "Zoom Meeting",
    }
}

-- overrides

for _, hostname in ipairs(hs.host.names()) do
    if string.find(hostname, "pixmac516") then
    end
end
