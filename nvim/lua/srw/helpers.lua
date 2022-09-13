local M = {}

function M.save_session()
    vim.api.nvim_command([[ mksession! ]])
    print("Session saved")
end

return M
