local VOLUME_SHIFT = 5

function round(number)
    return math.floor(number + 0.5)
end

function makeShiftFunction(diff)
    return function()
        local dev = hs.audiodevice.defaultOutputDevice()
        local currentVolume = dev:volume()
        local newVolume = round(math.max(
            math.min(currentVolume + diff, 100),
            0))
        dev:setVolume(newVolume)
        hs.alert.show("New volume: " .. newVolume .. "%", {}, 0.5)
    end
end

-- hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'right', makeShiftFunction(5))
-- hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'left', makeShiftFunction(-5))
