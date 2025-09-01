vim.loader.enable(true)

local function is_dark_mode()
    local cmd = "defaults read -g AppleInterfaceStyle >/dev/null 2>&1"
    return os.execute(cmd) == 0
end

vim.g.get_is_dark_mode = is_dark_mode
vim.g.is_dark_mode = is_dark_mode()
vim.g.mapleader = " "

vim.opt.autowrite = true
vim.opt.backspace = { "indent", "eol", "start" }
vim.opt.backup = false
vim.opt.backupcopy = "auto"
vim.opt.backupdir = { "~/.vim/backup" }
vim.opt.breakindent = true
vim.opt.complete = { ".", "w", "b", "u", "t", "i" }
vim.opt.completeopt = { "menuone", "preview" }
vim.opt.conceallevel = 0
vim.opt.cursorline = false
vim.opt.expandtab = true
vim.opt.formatoptions = "jtcroql"
vim.opt.gdefault = true
vim.opt.grepprg = "rg --vimgrep"
vim.opt.hidden = true
vim.opt.history = 50
vim.opt.hlsearch = false
vim.opt.ignorecase = true
vim.opt.inccommand = "split"
vim.opt.incsearch = true
vim.opt.lazyredraw = false
vim.opt.laststatus = 2
vim.opt.linebreak = true
vim.opt.list = false
vim.opt.modeline = true
vim.opt.mouse = "a"
vim.opt.number = false
vim.opt.relativenumber = false
vim.opt.ruler = false
vim.opt.scrolloff = 8
vim.opt.shiftround = true
vim.opt.shiftwidth = 4
vim.opt.shortmess = "tToOFIWa"
vim.opt.showcmd = true
vim.opt.signcolumn = "yes"
vim.opt.smartcase = true
vim.opt.smartindent = true
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.swapfile = true
vim.opt.switchbuf = { "useopen", "uselast" }
vim.opt.synmaxcol = 1024
vim.opt.tabstop = 4
vim.opt.termguicolors = true
vim.opt.textwidth = 0
vim.opt.timeoutlen = 500
vim.opt.ttimeoutlen = 10
vim.opt.undofile = true
vim.opt.updatetime = 4000
vim.opt.wildmode = { "list:longest", "list:full" }
vim.opt.winwidth = 80
vim.opt.wrap = false
vim.opt.writebackup = false
vim.opt.guicursor = ""
vim.opt.tags = { '.tags', '.git/tags' }
vim.opt.winborder = 'rounded'

vim.keymap.set('n', '<leader>o', ':update<Cr> :source<Cr>')
vim.keymap.set('n', '<leader>w', ':update<Cr>')
vim.keymap.set('n', '<leader>q', ':quit<Cr>')

vim.pack.add({
    { src = "https://github.com/stevearc/oil.nvim" },
    { src = "https://github.com/echasnovski/mini.pick" },
    { src = "https://github.com/catppuccin/nvim" },
    { src = "https://github.com/projekt0n/github-nvim-theme" },
    { src = 'https://github.com/neovim/nvim-lspconfig' },
    { src = "https://github.com/nvim-treesitter/nvim-treesitter", version = "main" },
    { src = "https://github.com/mason-org/mason.nvim" },
})

require("mini.pick").setup()
require("oil").setup()
require("mason").setup()

vim.keymap.set("n", "<leader>f", ":Pick files<cr>")
vim.keymap.set("n", "<leader>ht", ":Pick help<cr>")
vim.keymap.set("n", "<leader><leader>", ":Pick grep_live<cr>")
vim.keymap.set("n", "-", ":Oil<cr>")
vim.keymap.set("n", "<leader>y", vim.lsp.buf.format)

vim.lsp.enable({
    "lua_ls",
    "rust_analyzer",
    "ruff",
})

local function load_theme()
    if vim.g.is_dark_mode then
        vim.cmd.colorscheme "catppuccin-mocha"

        vim.cmd.highlight({ "TreesitterContextBottom", "gui=none" })
        vim.cmd.highlight({ "CursorLine", "guibg=#303347" })
        vim.cmd.highlight({ "CursorColumn", "guibg=#303347" })
    else
        vim.cmd.colorscheme "github_light"

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
end

vim.api.nvim_create_autocmd("Signal", {
    pattern = "SIGUSR1",
    callback = function()
        vim.g.is_dark_mode = vim.g.get_is_dark_mode()
        load_theme()
    end,
})
vim.cmd('set completeopt+=noselect')

vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(ev)
        local client = vim.lsp.get_client_by_id(ev.data.client_id)
        if client and client:supports_method('textDocument/completion') then
            vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = true })
        end
    end,
})

load_theme()
