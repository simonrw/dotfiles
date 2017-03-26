-- Disable all window animations when changing
hs.window.animationDuration = 0

-- Application launchers
for file in io.popen('find ~/.hammerspoon/config -name "*.lua" -exec basename {} \\; | sed "s/\\.lua$//g"'):lines() do
    require('config/' .. file)
end

-- Change brightnesses
current_brightness = hs.screen.mainScreen():getBrightness()
brightness_change = 0.1
function handleWindowChange(name, notify_type, application)

    if notify_type ~= hs.application.watcher.activated then
        return
    end

    -- Do not change anything if there is an external display plugged in
    local all_screens = hs.screen.allScreens()
    if #all_screens ~= 1 then
        return
    end

    if name == 'iTerm2' then
        local screen = hs.screen.find("Color LCD")
        current_brightness = screen:getBrightness()
        local new_brightness = math.max(math.min(current_brightness + brightness_change, 1.0), 0.0)
        screen:setBrightness(new_brightness)
    else
        screen:setBrightness(current_brightness)
    end
end

-- watcher = hs.application.watcher.new(handleWindowChange)
-- watcher:start()

function changeScreenResolution(target)
    local screen = hs.screen.find("Color LCD")
    if not screen:setMode(target.width, target.height, target.scale) then
        hs.alert.show('Could not set screen mode to: ' .. hs.inspect.inspect(target))
    end
end

function getWindowMode()
    local screen = hs.screen.find("Color LCD")
    return screen:currentMode()
end

function nextMode(direction)
    local currentMode = getWindowMode()
    if direction == 'up' then
        if currentMode.w == 1440 then
            return {width = 1680, height = 1050, scale = 2.0}
        elseif currentMode.w == 1680 then
            return {width = 1920, height = 1200, scale = 2.0}
        else
            return nil
        end
    elseif direction == 'down' then
        if currentMode.w == 1920 then
            return {width = 1680, height = 1050, scale = 2.0}
        elseif currentMode.w == 1680 then
            return {width = 1440, height = 900, scale = 2.0}
        else
            return nil
        end
    end

    return nil
end

-- Change screen resolution
hs.hotkey.bind({'cmd', 'ctrl', 'alt'}, 'Up', function()
    local target = nextMode('up')
    if target then
        changeScreenResolution(target)
    end
end)

hs.hotkey.bind({'cmd', 'ctrl', 'alt'}, 'Down', function()
    local target = nextMode('down')
    if target then
        changeScreenResolution(target)
    end
end)

-- Reload the config on file change
hs.pathwatcher.new(os.getenv('HOME') .. '/.hammerspoon/', function(files)
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
