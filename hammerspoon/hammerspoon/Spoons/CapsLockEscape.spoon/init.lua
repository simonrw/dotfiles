local obj = {}
obj.__index = obj

-- metadata
obj.name = "CapsLockEscape"
obj.version = "0.1"
obj.author = "Simon Walker <s.r.walker101@googlemail.com>"

local CANCEL_DELAY_SECONDS = 0.1

function obj:init()
    self.lastModifiers = {}
    self.sendEscape = false

    self.controlKeyTimer = hs.timer.delayed.new(CANCEL_DELAY_SECONDS, function()
        self.sendEscape = false
    end)

    self.controlTap = hs.eventtap.new({
        hs.eventtap.event.types.flagsChanged,
    }, function(event)
        local newModifiers = event:getFlags()
        if self.lastModifiers['ctrl'] == newModifiers['ctrl'] then
            return false
        end

        if self.lastModifiers['ctrl'] then
            if self.sendEscape then
                hs.eventtap.keyStroke({}, 'escape', 1)
            end
            self.controlKeyTimer:stop()
        else
            self.sendEscape = true
            self.controlKeyTimer:start()
        end
        self.lastModifiers = newModifiers
        return false
    end)

    self.keyDownEventTap = hs.eventtap.new({
        hs.eventtap.event.types.keyDown
    }, function(event)
            self.sendEscape = false
            return false
    end)
end

function obj:start()
    self.controlTap:start()
    self.keyDownEventTap:start()
end

function obj:stop()
    self.controlTap:stop()
    self.keyDownEventTap:stop()
end

return obj
