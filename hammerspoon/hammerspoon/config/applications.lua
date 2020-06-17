applications = {
    terminal = "Alacritty",
    browser = "Firefox",
    email = "Mail",
    chat = "Slack",
    music = "Spotify",
    documentation = "Dash",
    notes = "Notable",
    editor = "Visual Studio Code",
}

-- overrides

if string.find(hs.host.names()[1], "pixmac516") then
    applications.browser = "Google Chrome"
end
