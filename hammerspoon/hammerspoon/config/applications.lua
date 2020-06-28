applications = {
    terminal = "Alacritty",
    browser = "Google Chrome",
    email = "Mail",
    chat = "Slack",
    music = "Spotify",
    documentation = "Dash",
    notes = "Notable",
    editor = "Visual Studio Code",
}

-- overrides

for _, hostname in ipairs(hs.host.names()) do
    if string.find(hostname, "pixmac516") then
        -- host overrides
    end
end
