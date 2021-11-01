function StatusLine(mode)
    local result = {}
    local levels = {
        errors = "Error",
        warnings = "Warning",
        info = "Information",
        hints = "Hint",
    }

    local text = "| "
    local should_show = false
    for k, level in pairs(levels) do
        count = vim.lsp.diagnostic.get_count(0, level)
        if count > 0 then
            should_show = true
        end

        if count > 0 then
            text = table.concat({ text, string.format(" %s:%s", string.sub(level, 1, 1), count) })
        end
    end

    if mode == 'active' then
        return table.concat({
            -- left
            "%<%f",
            " ",
            "%h%m%r",
            -- diagnostics
            should_show and text or "",
            "%=",
            -- right
            "%-14.(%l,%c%V%) %P",
        })
    else
        return "%F %="
    end
end

-- vim.opt.statusline = [[%<%f a %h%m%r%=%-14.(%l,%c%V%) %P]]
vim.api.nvim_exec([[
augroup StatusLine
au!
au WinEnter,BufEnter * setlocal statusline=%!v:lua.StatusLine('active')
au WinLeave,BufLeave * setlocal statusline=%!v:lua.StatusLine('inactive')
augroup END
]], false)
