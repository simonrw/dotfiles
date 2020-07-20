applications = {
   terminal = {
	  name = "iTerm",
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
	  name = "Emacs",
      normal_size = {
          width = 768,
          height = 1024,
      },
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
