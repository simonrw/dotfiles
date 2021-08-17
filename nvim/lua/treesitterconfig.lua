local function setup()
    local ts = require('nvim-treesitter.configs')

    ts.setup {
        ensure_installed = 'maintained',
        highlight = {
            enable = true
        },
    }
end

if vim.fn.has('nvim-0.5') and vim.g.include_treesitter then
    setup()
end

