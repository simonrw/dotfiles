applications = {
    terminal = {
        name = "Kitty",
    },
    browser = {
        name = "Brave Browser",
    },
    email = {
        name = "Mail",
    },
    music = {
        name = "Spotify",
    },
    documentation = {
        name = "Dash",
    },
    notes = {
        name = "VimR",
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
    }
}

-- overrides

for _, hostname in ipairs(hs.host.names()) do
    if string.find(hostname, "pixmac516") then
    end
end
