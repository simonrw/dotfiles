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
    { src = "https://github.com/nvim-treesitter/nvim-treesitter",        version = "master" },
    { src = "https://github.com/mason-org/mason.nvim" },
    { src = "https://github.com/tpope/vim-fugitive" },
    { src = "https://github.com/tpope/vim-rhubarb" },
    { src = "https://github.com/lewis6991/gitsigns.nvim" },
    { src = "https://github.com/nvim-treesitter/nvim-treesitter-context" },
})

require('nvim-treesitter.configs').setup({
    ensure_installed = { "rust", "python" },
    highlight = {
        enable = true,
    },
})
require('treesitter-context').setup({ max_lines = 3 })
require("mini.pick").setup({
    options = {
        content_from_bottom = true,
    },
})
require("oil").setup()
require("mason").setup()

local setkey = function(key, action, modes, options)
    modes = modes or "n"
    options = options or { noremap = true, silent = true }
    vim.keymap.set(modes, key, action, options)
end

vim.diagnostic.config({
    virtual_text = false,
    signs = true,
    underline = false,
})

setkey("<leader>f", function() require("mini.pick").builtin.files() end)
setkey("<leader>F", function()
    require("mini.pick").builtin.files({ tool = "git" })
end)
setkey("<leader>ht", function() require("mini.pick").builtin.help() end)
setkey("gb", function() require("mini.pick").builtin.buffers() end)
setkey("<leader><leader>", function() require("mini.pick").builtin.grep_live() end)
setkey("-", ":Oil<cr>")
setkey("<leader>y", vim.lsp.buf.format)
setkey("<C-h>", "<C-w><C-h>")
setkey("<C-j>", "<C-w><C-j>")
setkey("<C-k>", "<C-w><C-k>")
setkey("<C-l>", "<C-w><C-l>")
setkey("<leader>A", vim.diagnostic.setqflist)
setkey("<leader>W", ':mksession!<cr> :echo "Session saved"<cr>')
setkey("<Esc>", "<C-\\><C-n>", { "t" })
setkey('<leader>gc', ':Git commit -v<cr>')
setkey('<leader>gd', ':Gvdiff<cr>')
setkey('<leader>gw', ':Gwrite<cr>')
setkey('<leader>gr', ':Gread<cr>')
setkey('<leader>ga', ':Git commit -v --amend<cr>')
setkey('gs', ':Git<cr>')
setkey(']c', function()
    if vim.wo.diff then return "]c" end
    vim.schedule(function()
        require('gitsigns').next_hunk()
    end)
    return '<Ignore>'
end)
setkey('[c', function()
    if vim.wo.diff then return "[c" end
    vim.schedule(function()
        require('gitsigns').prev_hunk()
    end)
    return '<Ignore>'
end)

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
setkey("Q", ":ToggleList<cr>")


vim.api.nvim_create_user_command("T", "split | resize 30 | term <args>", {
    complete = "shellcmd",
    force = true,
    nargs = "*",
})

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

vim.api.nvim_create_autocmd('TermOpen', {
    callback = function()
        vim.defer_fn(function()
            if vim.api.nvim_get_option_value('buftype', { buf = 0 }) == 'terminal' then
                vim.cmd([[startinsert]])
            end
        end, 100)
    end,
})

vim.api.nvim_create_autocmd('TextYankPost', {
    callback = function()
        if vim.fn.has('nvim-0.11') then
            require('vim.hl').on_yank()
        else
            require('vim.highlight').on_yank()
        end
    end,
})

vim.api.nvim_create_autocmd('BufReadPost', {
    pattern = { '*' },
    desc = 'When editing a file, always jump to the last known cursor position',
    callback = function()
        local line = vim.fn.line '\'"'
        if
            line >= 1
            and line <= vim.fn.line '$'
            and (vim.bo.filetype ~= 'commit') and (vim.bo.filetype ~= "gitcommit")
            and vim.fn.index({ 'xxd', 'gitrebase' }, vim.bo.filetype) == -1
        then
            vim.cmd 'normal! g`"'
        end
    end,
})

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
