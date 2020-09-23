-- Disable all window animations when changing
hs.window.animationDuration = 0

-- Application launchers
for file in io.popen('find ~/.hammerspoon/config -name "*.lua" -exec basename {} \\; | sed "s/\\.lua$//g"'):lines() do
    require('config/' .. file)
end

-- Reload the config on file change
configWatcher = hs.pathwatcher.new(os.getenv('HOME') .. '/.hammerspoon/', function(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == '.lua' then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end):start()

hs.alert.show('Config reloaded')
