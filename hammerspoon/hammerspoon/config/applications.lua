applications = {
   terminal = {
	  name = "Alacritty",
	  normal_size = {
		 width = 1024,
		 height = 768,
	  },
   },
   browser = {
	  name = "Firefox",
      normal_size = {
          width = 1920,
          height = 1200,
      },
   },
   email = {
	  name = "Thunderbird",
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
	  name = "Emacs",
      normal_size = {
          width = 768,
          height = 1024,
      },
   },
   editor = {
	  name = "Visual Studio Code",
   },
}

-- overrides

for _, hostname in ipairs(hs.host.names()) do
    if string.find(hostname, "pixmac516") then
        -- host overrides
    end
end
