local M = {}

M.package = { src = "https://github.com/catppuccin/nvim", name = "catppuccin" }

function M.is_dark_mode()
    -- Rex's long-running server can have a stale or missing shell theme variable.
    local ok, result = pcall(function()
        return vim.system({ vim.env.HOME .. "/.bin/is-dark-theme" }):wait(1000)
    end)
    if ok and (result.code == 0 or result.code == 1) then
        return result.code == 0
    end
    return vim.env.__IS_DARK_THEME ~= "0"
end

function M.load()
    vim.o.background = M.is_dark_mode() and "dark" or "light"
    -- Copy mode loads only the installed theme, not the rest of the editor's plugins.
    vim.cmd.packadd { M.package.name, bang = true }
    if vim.o.background == "dark" then
        vim.cmd.colorscheme "catppuccin-macchiato"

        vim.cmd.highlight({ "TreesitterContextBottom", "gui=none" })
        vim.cmd.highlight({ "CursorLine", "guibg=#363a4f" })
        vim.cmd.highlight({ "CursorColumn", "guibg=#363a4f" })
        vim.cmd.highlight({ "LineNr", "guifg=#6e738d" })
    else
        vim.cmd.colorscheme "catppuccin-latte"

        vim.cmd.highlight({ "DiagnosticError", "guifg=Red" })
        vim.cmd.highlight({ "DiagnosticHint", "guifg=Orange" })
        vim.cmd.highlight({ "DiagnosticWarn", "guifg=Orange" })
        vim.cmd.highlight({ "DiagnosticInfo", "guifg=LightBlue" })
        vim.cmd.highlight({ "DiagnosticFloatingError", "guifg=Red" })
        vim.cmd.highlight({ "DiagnosticFloatingHint", "guifg=Orange" })
        vim.cmd.highlight({ "DiagnosticFloatingInfo", "guifg=LightBlue" })
        vim.cmd.highlight({ "DiagnosticFloatingWarn", "guifg=Orange" })
        vim.cmd.highlight({ "DiagnosticVirtualTextError", "guifg=Red" })
        vim.cmd.highlight({ "DiagnosticVirtualTextHint", "guifg=Orange" })
        vim.cmd.highlight({ "DiagnosticVirtualTextInfo", "guifg=LightBlue" })
        vim.cmd.highlight({ "DiagnosticVirtualTextWarn", "guifg=Orange" })
        vim.cmd.highlight({ "Comment", "guifg=#e69340" })
        vim.cmd.highlight({ "TreesitterContext", "guibg=#f0f0f0" })
    end

    -- touchup.nvim defines heading underlines with `default = true`, so
    -- catppuccin's own heading groups override them. Re-apply the underline
    -- while preserving catppuccin's heading colors.
    for group, extra in pairs({
        ["@markup.heading.1.markdown"] = { bold = true, underline = true },
        ["@markup.heading.2.markdown"] = { underline = true },
    }) do
        local hl = vim.api.nvim_get_hl(0, { name = group, link = false })
        vim.api.nvim_set_hl(0, group, vim.tbl_extend("force", hl, extra))
    end
end

function M.setup()
    M.load()
    vim.api.nvim_create_autocmd("Signal", {
        group = vim.api.nvim_create_augroup("DotfilesTheme", { clear = true }),
        pattern = "SIGUSR1",
        callback = M.load,
    })
end

return M
