local ts = require('nvim-treesitter.configs')

ts.setup {
    ensure_installed = {
        'bash',
        'c',
        'go',
        'graphql',
        'html',
        'javascript',
        'json',
        'lua',
        'php',
        'python',
        'rust',
        'svelte',
        'toml',
        'tsx',
        'typescript',
        'yaml',
        'zig',
    },
    highlight = {
        enable = true
    },
}


