vim.api.nvim_create_user_command("T", "split | resize 30 | term <args>", {
    complete = "shellcmd",
    force = true,
    nargs = "*",
})

vim.api.nvim_create_user_command("ToggleList", function()
    local qf_exists = false
    for _, win in pairs(vim.fn.getwininfo()) do
        if win["quickfix"] == 1 then
            qf_exists = true
        end
    end

    if qf_exists then
        vim.cmd "cclose"
    else
        vim.cmd "copen"
    end
end, {})

vim.api.nvim_create_user_command("W", "write", {})

vim.api.nvim_create_user_command("Mkdir", function()
    -- full path of current file
    local filepath = vim.fn.expand('%:p')
    if filepath == '' then
        return
    end

    -- parent directory
    local dir = vim.fn.fnamemodify(filepath, ':h')

    -- mkdir -p behavior: only create if not exists
    if vim.fn.isdirectory(dir) == 0 then
        vim.fn.mkdir(dir, 'p')
    end
end, {})
