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

-- Emulate caffeine
local caffeine = hs.menubar.new()
function setCaffeineDisplay(state)
    local result
    if state then
        result = caffeine:setIcon("caffeine-icons/active@2x.png")
    else
        result = caffeine:setIcon("caffeine-icons/inactive@2x.png")
    end
end

function caffeineClicked()
    setCaffeineDisplay(hs.caffeinate.toggle("displayIdle"))
end

if caffeine then
    caffeine:setClickCallback(caffeineClicked)
    setCaffeineDisplay(hs.caffeinate.get("displayIdle"))
end

function changeScreenResolution(target)
    local screen = hs.screen.find("Color LCD")
    if not screen:setMode(target.width, target.height, target.scale) then
        hs.alert.show('Could not set screen mode to: ' .. hs.inspect.inspect(target))
    end
end

-- Change screen resolution
hs.hotkey.bind({'cmd', 'ctrl', 'alt'}, 'Up', function()
    local target = {
        width = 1680,
        height = 1050,
        scale = 2.0,
    }
    changeScreenResolution(target)
end)

hs.hotkey.bind({'cmd', 'ctrl', 'alt'}, 'Down', function()
    local screen = hs.screen.find("Color LCD")
    local target = {
        width = 1440,
        height = 900,
        scale = 2.0,
    }
    changeScreenResolution(target)
end)

watcher = hs.application.watcher.new(handleWindowChange)
watcher:start()

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
