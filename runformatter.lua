local function RunRuff()
    if vim.fn.executable("ruff") then
        local filename = vim.fn.expand("%")
        vim.fn.jobstart({ "ruff", "format", filename }, {
            stdout_buffered = true,
            on_exit = function()
                vim.cmd.edit(filename)
            end,
        })
    end
end

local group = vim.api.nvim_create_augroup("PythonRuff", { clear = true })
vim.api.nvim_create_autocmd({ "BufWritePost" }, {
    pattern = { "*.py" },
    callback = function()
        RunRuff()
    end,
    group = group,
})
