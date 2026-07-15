local M = {}

local is_work = function()
    local hostname = hs.host.localizedName()
    return hostname == 'localstack-simonwalker'
end

M.is_work = is_work

return M
