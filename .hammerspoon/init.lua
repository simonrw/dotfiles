-- Disable all window animations when changing
hs.window.animationDuration = 0

for file in io.popen('find ~/.hammerspoon/config -name "*.lua" -exec basename {} \\; | sed "s/\\.lua$//g"'):lines() do
    require('config/' .. file)
end

hs.loadSpoon("ReloadConfiguration")
spoon.ReloadConfiguration:start()

hs.alert.show('Config reloaded')
