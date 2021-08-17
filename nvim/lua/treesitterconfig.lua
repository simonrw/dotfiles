if not vim.fn.has('nvim-0.5') or not vim.g.include_treesitter then
    return
end

local ts = require('nvim-treesitter.configs')

ts.setup {
    ensure_installed = 'maintained',
    highlight = {
        enable = true
    },
}


