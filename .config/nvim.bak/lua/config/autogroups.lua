do
    local autogroups = {
        ["diff-mode"] = {clear = true},
        ["last-position"] = {clear = true},
        ["lua-highlight"] = {clear = true},
        ["nixvim_binds_LspAttach"] = {clear = true},
        ["terminal-settings"] = {clear = true}
    }

    for group_name, options in pairs(autogroups) do
        vim.api.nvim_create_augroup(group_name, options)
    end
end
