local prefix = "ctrl+s>"

rex.bind(prefix .. "s", "pane.send_key", { key = "ctrl+s" })
rex.bind(prefix .. "ctrl+z", function() return true end)

rex.bind(prefix .. "c", "client.tab.new")
rex.bind(prefix .. "n", "client.tab.next")
rex.bind(prefix .. "p", "client.tab.previous")
rex.bind(prefix .. "ctrl+n", "client.tab.next")
rex.bind(prefix .. "ctrl+p", "client.tab.previous")
rex.bind(prefix .. "9", "session.previous")
rex.bind(prefix .. "0", "session.next")

for index = 1, 8 do
	rex.bind(prefix .. index, "client.tab.goto", { index = index })
end

rex.bind(prefix .. "shift+\\", "pane.split", { direction = "right" })
rex.bind(prefix .. "-", "pane.split", { direction = "down" })
rex.bind(prefix .. "i", "pane.split", { direction = "auto" })
rex.bind(prefix .. "z", "pane.zoom")
rex.bind(prefix .. "x", "pane.close")
rex.bind(prefix .. "b", "pane.move_to_new_tab")

for key, direction in pairs({ h = "left", j = "down", k = "up", l = "right" }) do
	rex.bind(prefix .. key, "pane.focus", { direction = direction })
	-- Rex resizes by percentage, not tmux's five terminal cells.
	rex.bind(prefix .. "shift+" .. key, "pane.resize", { direction = direction, amount = 5 })
end

rex.bind(prefix .. "r", "client.config.reload")

rex.bind(prefix .. "f", "session.switch")
