local ts = require('nvim-treesitter.configs')

ts.setup {
    ensure_installed = {'python', 'rust', 'lua', 'zig', 'go', 'c', 'html', 'javascript', 'typescript', 'toml'},
    highlight = {
        enable = true
    },
}


