-- FrameCache "class"
FrameCache = {}
FrameCache.__index = FrameCache

function FrameCache:new()
    local props = {}
    setmetatable(props, FrameCache)
    props.history = {}
    return props
end

function FrameCache:add(win)
    local app = win:application()
    local name = app:name()
    local frame = win:frame()

    if self.history[name] == nil then
        self.history[name] = {}
    end

    if not self:lastCachedPositionEqual(frame, self.history[name]) then
        table.insert(self.history[name], frame)
    end
end

function FrameCache:lastCachedPositionEqual(frame, cache_entry)
    local prev = cache_entry[#cache_entry]
    return prev == frame
end

function FrameCache:pop(name)
    local win = hs.window.focusedWindow()

    if self.history[name] == nil then
        return
    end

    if #self.history[name] == 0 then
        return
    end

    local newFrame = table.remove(self.history[name])
    win:setFrame(newFrame)
end
