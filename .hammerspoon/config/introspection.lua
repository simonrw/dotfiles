local M = {}

local is_work = function()
    local hostname = hs.host.localizedName()
    return hostname == 'walker-s'
end

M.is_work = is_work
M.hostname = hs.host.localizedName

return M
