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
vim.opt.completeopt = { "menuone", "popup" }
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
vim.opt.number = true
vim.opt.relativenumber = true
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

vim.keymap.set('n', 'cp', ':0,$y+<cr>')
vim.keymap.set('n', '<leader>o', ':update<Cr> :source ~/.config/nvim/init.lua<Cr>')
vim.keymap.set('n', '<leader>w', ':update<Cr>')
vim.keymap.set('n', '<leader>q', ':quit<Cr>')

vim.pack.add({
    { src = "https://github.com/stevearc/oil.nvim" },
    -- dependency of telescope
    { src = "https://github.com/nvim-lua/plenary.nvim" },
    { src = "https://github.com/nvim-telescope/telescope.nvim",              version = "0.1.x" },
    { src = "https://github.com/catppuccin/nvim",                            name = "catppuccin" },
    { src = "https://github.com/projekt0n/github-nvim-theme" },
    { src = 'https://github.com/neovim/nvim-lspconfig' },
    { src = "https://github.com/nvim-treesitter/nvim-treesitter",            version = "master" },
    { src = "https://github.com/tpope/vim-fugitive" },
    { src = "https://github.com/christoomey/vim-conflicted" },
    { src = "https://github.com/tpope/vim-rhubarb" },
    { src = "https://github.com/lewis6991/gitsigns.nvim" },
    { src = "https://github.com/nvim-treesitter/nvim-treesitter-context" },
    { src = "https://github.com/vim-test/vim-test" },
    { src = "https://github.com/nvim-mini/mini.surround" },
    { src = "https://github.com/MeanderingProgrammer/render-markdown.nvim" },
    { src = "https://github.com/folke/zen-mode.nvim" },
    { src = "https://github.com/nvim-treesitter/nvim-treesitter-textobjects" },
})

function parse_grep_nul(s)
    if type(s) ~= "string" then
        return nil
    end
    if not s:find("\0", 1, true) then
        return s
    end

    local parts = {}
    local start = 1
    while true do
        local idx = s:find("\0", start, true) -- plain find
        if not idx then
            -- take trailing tail only if we actually saw at least one NUL;
            -- grep-live format always has 3 NULs, so tail isn't needed
            break
        end
        local splitstr = s:sub(start, idx - 1)
        table.insert(parts, splitstr)
        start = idx + 1
    end

    -- insert the rest of the string
    table.insert(parts, s:sub(start, -1))

    if #parts < 4 then
        return nil
    end

    local filename = parts[1]
    local lnum = tonumber(parts[2]) or 1
    local col = tonumber(parts[3]) or 1
    local text = parts[4] or ""

    if filename == "" then
        return nil
    end

    return {
        filename = filename,
        lnum = lnum,
        col = col,
        text = text,
    }
end

local setkey = function(key, action, modes, options)
    modes = modes or "n"
    options = options or { noremap = true, silent = true }
    vim.keymap.set(modes, key, action, options)
end

setkey("<leader>f", function() require("telescope.builtin").git_files() end)
setkey("<leader>F", function() require("telescope.builtin").find_files() end)
setkey("<leader>j", function() require("telescope.builtin").jumplist() end)
setkey("<leader>ht", function() require("telescope.builtin").help_tags() end)
setkey("gb", function() require("telescope.builtin").buffers() end)
setkey("<leader><leader>", function() require("telescope.builtin").live_grep() end)
setkey("<leader>A", function() require("telescope.builtin").diagnostics() end)

setkey("-", ":Oil<cr>")
setkey("<leader>y", vim.lsp.buf.format)
setkey('<leader>r', vim.lsp.buf.rename)
setkey("<C-h>", "<C-w><C-h>")
setkey("<C-j>", "<C-w><C-j>")
setkey("<C-k>", "<C-w><C-k>")
setkey("<C-l>", "<C-w><C-l>")
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

vim.api.nvim_create_autocmd('FileType', {
    pattern = { 'markdown' },
    callback = function(ev)
        require('render-markdown').setup({
            latex = {
                enabled = false,
            },
        })
        local zen_mode = require('zen-mode')
        zen_mode.setup({
            window = {
                backdrop = 1,
            },
            on_open = function()
                vim.o.number = false
                vim.o.relativenumber = false
            end,
            on_close = function()
                vim.o.number = true
                vim.o.relativenumber = true
            end,
        })
    end,
})

setkey('yoz', function() require('zen-mode').toggle() end)

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

-- mappings to toggle settings
setkey('yow', function()
    vim.o.wrap = not (vim.o.wrap)
    vim.print('wrap: ' .. tostring(vim.o.wrap))
end)
setkey('yow', function()
    vim.o.wrap = not (vim.o.wrap)
    vim.print('wrap: ' .. tostring(vim.o.wrap))
end)
setkey('yon', function()
    vim.o.number = not (vim.o.number)
    vim.print('number: ' .. tostring(vim.o.number))
end)
setkey('yor', function()
    vim.o.relativenumber = not (vim.o.relativenumber)
    vim.print('relativenumber: ' .. tostring(vim.o.relativenumber))
end)
setkey('yos', function()
    vim.o.spell = not (vim.o.spell)
    vim.print('spell: ' .. tostring(vim.o.spell))
end)
setkey('yod', function()
    vim.o.diff = not (vim.o.diff)
    vim.print('diff: ' .. tostring(vim.o.diff))
end)


-- add custom mapping for git blame
setkey('yob', function()
    if vim.o.filetype ~= 'fugitiveblame' then
        vim.cmd([[Git blame]])
    end
end)

vim.api.nvim_create_autocmd('FileType', {
    pattern = { 'fugitiveblame' },
    callback = function(ev)
        vim.api.nvim_buf_set_keymap(ev.buf, 'n', 'yob', 'gq', {})
    end,
})

vim.api.nvim_create_autocmd('FileType', {
    pattern = { 'fugitive', 'fugitiveblame' },
    callback = function(ev)
        -- use this API since we want recursive mapping behaviour
        vim.api.nvim_buf_set_keymap(ev.buf, 'n', 'q', 'gq', {})
    end,
})


-- configure rust-analyzer
vim.lsp.config('rust_analyzer', {
    settings = {
        ['rust-analyzer'] = {
            cargo = {
                allFeatures = true,
            },
        },
    },
})

vim.lsp.enable({
    "lua_ls",
    "rust_analyzer",
    "ruff",
    "gopls",
    "ty",
})

local function load_theme()
    if vim.g.is_dark_mode then
        vim.cmd.colorscheme "catppuccin-macchiato"

        vim.cmd.highlight({ "TreesitterContextBottom", "gui=none" })
        vim.cmd.highlight({ "CursorLine", "guibg=#303347" })
        vim.cmd.highlight({ "CursorColumn", "guibg=#303347" })
        vim.cmd.highlight({ "LineNr", "guifg=#6c7086" })
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

-- configure vim-test
vim.g["test#python#runner"] = "pytest"
vim.g["test#python#pytest#executable"] = "pytest"
vim.g["test#javascript#reactscripts#options"] = "--watchAll=false"
vim.g["test#strategy"] = "basic"

vim.keymap.set("n", "tl", function()
    vim.cmd("update")
    vim.cmd("TestLast")
end, { noremap = true, silent = true, desc = "Run the last executed test" })
vim.keymap.set("n", "tn", function()
    vim.cmd("update")
    vim.cmd("TestNearest")
end, { noremap = true, silent = true, desc = "Run the nearest test" })
vim.keymap.set("n", "tf", function()
    vim.cmd("update")
    vim.cmd("TestFile")
end, { noremap = true, silent = true, desc = "Test the current file" })
vim.keymap.set("n", "ta", function()
    vim.cmd("update")
    vim.cmd("TestSuite")
end, { noremap = true, silent = true, desc = "Run the whole test suite" })

-- custom command to run test on AWS (LocalStack test)
vim.api.nvim_create_user_command("AwsTestNearest", function()
    vim.cmd([[TestNearest TEST_TARGET=AWS_CLOUD AWS_PROFILE=ls-sandbox SNAPSHOT_UPDATE=1]])
end, {})

-- lsp bindings
-- remove default lsp bindings
local is_mapped = function(mode, key)
    return vim.fn.maparg(key, mode) ~= ""
end

for _, mapping in ipairs({ 'grn', 'gra', 'grr', 'gri', 'grt' }) do
    if is_mapped(mapping) then
        vim.keymap.del('n', mapping)
    end
end

setkey('gd', vim.lsp.buf.definition)
setkey('<leader>gt', vim.lsp.buf.type_definition)
setkey('gr', function() require("telescope.builtin").lsp_references() end)
setkey('gi', vim.lsp.buf.implementation)
setkey('<leader>s', function() require("telescope.builtin").lsp_document_symbols() end)
setkey('<leader>a', vim.lsp.buf.code_action)

setkey('<c-space>', function()
    vim.lsp.completion.get()
end, { 'i' })

-- package management commands
local get_package_names = function()
    local packages = vim.pack.get()
    local package_names = {}
    for _, package in ipairs(packages) do
        table.insert(package_names, package.spec.name)
    end
    return package_names
end

vim.api.nvim_create_user_command('UpdatePackages', function(opts)
    local package_names = get_package_names()
    local fopts = {}
    if opts.bang then
        fopts.force = true
    end
    vim.pack.update(package_names, fopts)
end, { bang = true })

vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(ev)
        local client = vim.lsp.get_client_by_id(ev.data.client_id)
        if client and client:supports_method('textDocument/completion') then
            vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = true })
        end

        local show_diagnostic_for_line = function()
            local row, col = unpack(vim.api.nvim_win_get_cursor(0))
            local diagnostics = vim.diagnostic.get(0, { lnum = row - 1 })

            local messages = {}
            for _, diagnostic in ipairs(diagnostics) do
                for every in diagnostic.message:gmatch("[^\n]+") do
                    local message = every:gsub('%s+$', '')
                    table.insert(messages, message)
                end
            end


            local buf = vim.api.nvim_create_buf(false, true)
            vim.api.nvim_buf_set_lines(buf, 0, -1, false, messages)
            local opts = {
                relative = 'cursor',
                width = 75,
                height = #messages,
                col = 0,
                row = 1,
                anchor = 'NW',
                style = 'minimal',
            }
            local win = vim.api.nvim_open_win(buf, true, opts)
            vim.api.nvim_set_option_value('wrap', true, { win = win })
        end

        vim.keymap.set('n', '<leader>d', show_diagnostic_for_line)
    end,
})

local init_done_event_name = 'InitDone'

-- any lazy initialization
vim.api.nvim_create_autocmd('User', {
    pattern = init_done_event_name,
    callback = function()
        vim.diagnostic.config({
            virtual_text = false,
            signs = true,
            underline = true,
        })
        vim.lsp.inlay_hint.enable(true)

        require('nvim-treesitter.configs').setup({
            ensure_installed = {
                'hcl',
                'javascript',
                'terraform',
                'tsx',
                'typescript',
                'vim',
                'go',
                'json',
                'lua',
                'python',
                'rust',
                'yaml',
                'html',
                "jsonc",
            },
            highlight = {
                enable = true,
                additional_vim_regex_highlighting = false,
            },
            textobjects = {
                select = {
                    enable = true,
                    keymaps = {
                        ["af"] = "@function.outer",
                        ["if"] = "@function.inner",
                        ["ac"] = "@class.outer",
                        ["ic"] = "@class.inner",
                    },
                    selection_modes = {
                        ['@function.inner'] = 'V',
                        ['@class.inner'] = 'V',
                        ['@function.outer'] = 'V',
                        ['@class.outer'] = 'V',
                    },
                }
            },
        })
        require('treesitter-context').setup({ max_lines = 3 })
        require('mini.surround').setup()

        require("oil").setup()
    end,

    -- TODO: make this on file open/read
    -- require('modules/runtests').setup()
})


load_theme()

-- finally emit the config loaded event for lazy initialization
vim.schedule(function()
    vim.api.nvim_exec_autocmds('User', { pattern = init_done_event_name, modeline = false })
end)
