applications = {
    terminal = {
        name = "Alacritty",
    },
    browser = {
        name = "Firefox Developer Edition",
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
        name = "VimR",
    },
    editor = {
        name = "Emacs",
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
        applications.browser.name = "Google Chrome"
    end
end
