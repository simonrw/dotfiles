vim.loader.enable(true)

local function is_dark_mode()
    local is_dark_theme = vim.env.__IS_DARK_THEME
    return is_dark_theme == nil or is_dark_theme == "1"
end

vim.g.get_is_dark_mode = is_dark_mode
vim.g.is_dark_mode = is_dark_mode()
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

local native_completion_disabled_filetypes = {
    "markdown",
    "gitcommit",
    "octo",
    "jjdescription",
}

local function native_completion_enabled(bufnr)
    return not vim.tbl_contains(native_completion_disabled_filetypes, vim.bo[bufnr].filetype)
end

-- disable editorconfig as it's not providing value
vim.g.editorconfig = false

vim.opt.autowrite = true
vim.opt.backspace = { "indent", "eol", "start" }
vim.opt.backup = false
vim.opt.backupcopy = "auto"
vim.opt.backupdir = { "~/.vim/backup" }
vim.opt.breakindent = true
vim.opt.complete = { ".", "w", "b", "u", "i" }
vim.opt.completeopt = { "fuzzy", "menuone", "popup", "noselect" }
vim.opt.conceallevel = 0
vim.opt.cursorline = false
vim.opt.pumheight = 7
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
vim.opt.laststatus = 3
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
vim.opt.shortmess = "ctToOFIWa"
vim.opt.showcmd = true
vim.opt.signcolumn = "yes"
vim.opt.smartcase = true
vim.opt.smartindent = true
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.swapfile = false
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
vim.opt.tags = { '.tags', '.git/tags' }

-- options added in nvim 0.12
if vim.fn.has('nvim-0.12') == 1 then
    vim.opt.autocomplete = true
    vim.opt.complete = { "o", ".", "w", "b", "u", "i" }
    vim.opt.pummaxwidth = 80
    vim.opt.winborder = 'rounded'
    vim.opt.confirm = true

    -- add some default built-in packages
    vim.cmd.packadd('cfilter')
    vim.cmd.packadd('nvim.undotree')

    vim.api.nvim_create_autocmd("FileType", {
        pattern = native_completion_disabled_filetypes,
        callback = function(ev)
            vim.bo[ev.buf].autocomplete = false
        end,
    })
end

vim.keymap.set('n', 'cp', ':0,$y+<cr>')
vim.keymap.set('n', '<leader>w', ':update<Cr>')
vim.keymap.set('n', '<leader>q', ':quit<Cr>')

if vim.fn.has('nvim-0.12') ~= 1 or not vim.pack then
    error("This config requires Neovim 0.12 or newer for vim.pack package management.")
end

local gh = function(repo)
    return "https://github.com/" .. repo
end

vim.api.nvim_create_autocmd('PackChanged', {
    group = vim.api.nvim_create_augroup('PackHooks', { clear = true }),
    callback = function(ev)
        local data = ev.data or {}
        local spec = data.spec or {}
        if spec.name == 'nvim-treesitter' and (data.kind == 'install' or data.kind == 'update') then
            vim.schedule(function()
                pcall(vim.cmd, 'TSUpdate')
            end)
        end
    end,
})

vim.pack.add({
    { src = gh('stevearc/oil.nvim') },
    { src = gh('catppuccin/nvim'),                          name = 'catppuccin' },
    { src = gh('nvim-treesitter/nvim-treesitter'),          version = 'main' },
    { src = gh('neovim/nvim-lspconfig') },
    { src = gh('tpope/vim-fugitive') },
    { src = gh('tpope/vim-surround') },
    { src = gh('tpope/vim-repeat') },
    { src = gh('tpope/vim-rhubarb') },
    { src = gh('tpope/vim-dispatch') },
    { src = gh('christoomey/vim-conflicted') },
    { src = gh('lewis6991/gitsigns.nvim') },
    { src = gh('nvim-treesitter/nvim-treesitter-context') },
    { src = gh('vim-test/vim-test') },
    { src = gh('coder/claudecode.nvim') },
    { src = gh('MeanderingProgrammer/render-markdown.nvim') },
    { src = gh('simonrw/ask-agent') },
    { src = gh('folke/zen-mode.nvim') },
    { src = gh('hedyhli/outline.nvim') },
    { src = gh('ibhagwan/fzf-lua') },
    { src = gh('nvim-lua/plenary.nvim') },
    { src = gh('pwntester/octo.nvim') },
    { src = gh('mfussenegger/nvim-dap') },
    { src = gh('mfussenegger/nvim-dap-python') },
    { src = gh('igorlfs/nvim-dap-view') },
    { src = gh('barrettruth/diffs.nvim') },
    { src = gh('teamtype/teamtype-nvim') },
}, { confirm = false, load = true })

vim.api.nvim_create_user_command('VimUpdate', function(opts)
    if vim.fn.has('nvim-0.13') == 1 then
        vim.cmd('packupdate' .. (opts.bang and '!' or ''))
        return
    end

    vim.pack.update(nil, { force = opts.bang })
end, { bang = true })

if vim.env.NVIM_VIM_UPDATE == '1' then
    vim.cmd('VimUpdate!')
    vim.cmd('qa')
    return
end

require("oil").setup({})

local enabled_treesitter_languages = {
    'go',
    'hcl',
    'html',
    'javascript',
    'json',
    'lua',
    'nix',
    'python',
    'rust',
    'terraform',
    'tsx',
    'typescript',
    'vim',
    'xml',
    'yaml',
}
local nvim_treesitter = require("nvim-treesitter")
if type(nvim_treesitter.install) == "function" then
    nvim_treesitter.install(enabled_treesitter_languages)
else
    require("nvim-treesitter.configs").setup({
        ensure_installed = enabled_treesitter_languages,
    })
end

local function treesitter_first_node(match, capture_id)
    local node = match[capture_id]
    if type(node) == "table" then
        return node[1]
    end
    return node
end

local html_script_type_languages = {
    ["importmap"] = "json",
    ["module"] = "javascript",
    ["application/ecmascript"] = "javascript",
    ["text/ecmascript"] = "javascript",
}

local non_filetype_match_injection_language_aliases = {
    ex = "elixir",
    pl = "perl",
    sh = "bash",
    ts = "typescript",
    uxn = "uxntal",
}

local function parser_from_markdown_info_string(injection_alias)
    return vim.filetype.match({ filename = "a." .. injection_alias })
        or non_filetype_match_injection_language_aliases[injection_alias]
        or injection_alias
end

-- nvim-treesitter still registers these directives for the pre-0.13
-- single-node capture API, while nightly now passes capture node lists.
vim.treesitter.query.add_directive("set-lang-from-mimetype!", function(match, _, bufnr, pred, metadata)
    local node = treesitter_first_node(match, pred[2])
    if not node then
        return
    end

    local type_attr_value = vim.treesitter.get_node_text(node, bufnr)
    local parts = vim.split(type_attr_value, "/", {})
    metadata["injection.language"] = html_script_type_languages[type_attr_value] or parts[#parts]
end, { force = true })

vim.treesitter.query.add_directive("set-lang-from-info-string!", function(match, _, bufnr, pred, metadata)
    local node = treesitter_first_node(match, pred[2])
    if not node then
        return
    end

    local injection_alias = vim.treesitter.get_node_text(node, bufnr):lower()
    metadata["injection.language"] = parser_from_markdown_info_string(injection_alias)
end, { force = true })

vim.treesitter.query.add_directive("downcase!", function(match, _, bufnr, pred, metadata)
    local id = pred[2]
    local node = treesitter_first_node(match, id)
    if not node then
        return
    end

    local text = vim.treesitter.get_node_text(node, bufnr, { metadata = metadata[id] }) or ""
    metadata[id] = metadata[id] or {}
    metadata[id].text = string.lower(text)
end, { force = true })

vim.api.nvim_create_autocmd('FileType', {
    pattern = enabled_treesitter_languages,
    callback = function() vim.treesitter.start() end,
})

require("treesitter-context").setup({
    max_lines = 3,
})

require("claudecode").setup({
    auto_start = false,
    terminal = {
        provider = "none",
    },
})
vim.keymap.set('n', '<localleader>ac', '<cmd>ClaudeCode<cr>', { desc = "Toggle Claude" })
vim.keymap.set('n', '<localleader>ab', '<cmd>ClaudeCodeAdd %<cr>', { desc = "Add current buffer" })
vim.keymap.set('v', '<localleader>as', '<cmd>ClaudeCodeSend<cr>', { desc = "Send to Claude" })
vim.keymap.set('n', '<localleader>aa', '<cmd>ClaudeCodeDiffAccept<cr>', { desc = "Accept diff" })
vim.keymap.set('n', '<localleader>ad', '<cmd>ClaudeCodeDiffDeny<cr>', { desc = "Deny diff" })

require('zen-mode').setup({
    window = {
        backdrop = 1.0,
        options = {
            number = false,
            relativenumber = false,
        },
    },
    plugins = {
        options = {
            laststatus = 0,
        },
    },
})
vim.keymap.set('n', 'yoz', function() require('zen-mode').toggle() end, { desc = "Toggle zen mode" })

require("outline").setup({})
vim.keymap.set('n', '<leader>o', '<cmd>Outline<CR>', { desc = "Toggle outline" })

require("fzf-lua").setup({
    "telescope",
    winopts = {
        preview = {
            default = false,
        },
    },
    fzf_opts = {
        ["--layout"] = "reverse",
        ["--marker"] = "+",
        ["--gutter"] = " ",
        ["--cycle"] = true
    },
})
require("fzf-lua").register_ui_select()

require("octo").setup({
    picker = "fzf-lua",
    -- bare Octo command opens picker of commands
    enable_builtin = true,
    file_panel = {
        icons = false,
    },
})

require("dap-python").setup("uv")
require("dap-python").test_runner = "pytest"
require("dap-view").setup({})

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

setkey("<leader>f", function() require("fzf-lua").git_files() end)
setkey("<leader>F", function() require("fzf-lua").files() end)
setkey("<leader>j", function() require("fzf-lua").jumps() end)
setkey("<leader>ht", function() require("fzf-lua").helptags() end)
setkey("gb", function() require("fzf-lua").buffers() end)
setkey("<leader><leader>", function() require("fzf-lua").live_grep_native() end)
setkey("<leader>A", function() require("fzf-lua").diagnostics_workspace() end)

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
        if vim.fn.has('nvim-0.13') == 1 then
            vim.hl.hl_op()
        elseif vim.fn.has('nvim-0.11') == 1 then
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
    vim.cmd([[diffthis]])
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


-- configure LSP servers
vim.lsp.config["cfn-lsp"] = {
    cmd = { "cfn-lsp" },
    filetypes = { "yaml", "json" },
}

vim.lsp.config["sourcekit-lsp"] = {
    cmd = { 'sourcekit-lsp' },
    filetypes = { 'swift' },
    root_markers = {
        '.git',
        'compile_commands.json',
        '.sourcekit-lsp',
        'Package.swift',
    },
    get_language_id = function(_, ftype)
        return ftype
    end,
    capabilities = {
        workspace = {
            didChangeWatchedFiles = {
                dynamicRegistration = true,
            },
        },
        textDocument = {
            diagnostic = {
                dynamicRegistration = true,
                relatedDocumentSupport = true,
            },
        },
    },
}

vim.lsp.enable({
    "lua_ls",
    "rust_analyzer",
    "ruff",
    "gopls",
    "ty",
    "ts_ls",
    "cfn-lsp",
    "terraformls",
    "sourcekit-lsp",
})

local function load_theme()
    if vim.g.is_dark_mode then
        vim.cmd.colorscheme "catppuccin-mocha"

        vim.cmd.highlight({ "TreesitterContextBottom", "gui=none" })
        vim.cmd.highlight({ "CursorLine", "guibg=#303347" })
        vim.cmd.highlight({ "CursorColumn", "guibg=#303347" })
        vim.cmd.highlight({ "LineNr", "guifg=#6c7086" })
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
end

vim.api.nvim_create_autocmd("Signal", {
    pattern = "SIGUSR1",
    callback = function()
        vim.g.is_dark_mode = vim.g.get_is_dark_mode()
        load_theme()
    end,
})
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
vim.api.nvim_create_user_command("AwsTestNearest",
    "TestNearest TEST_TARGET=AWS_CLOUD AWS_PROFILE=ls-sandbox SNAPSHOT_UPDATE=1 <args>", {
        nargs = "*",
    })

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

setkey('gd', function() require("fzf-lua").lsp_definitions() end)
setkey('<leader>gt', function() require("fzf-lua").lsp_typedefs() end)
setkey('gr', function() require("fzf-lua").lsp_references() end)
setkey('gi', function() require("fzf-lua").lsp_implementations() end)
setkey('<leader>S', function() require("fzf-lua").lsp_document_symbols() end)
setkey('<leader>s', function() require("fzf-lua").lsp_workspace_symbols() end)
setkey('<leader>a', function() require("fzf-lua").lsp_code_actions() end)

-- dap keymaps (td prefix matches .ideavimrc)
setkey('tdb', function() require('dap').toggle_breakpoint() end)
setkey('tdv', function() require('dap').set_breakpoint(vim.fn.input('Breakpoint condition: ')) end)
setkey('tdj', function() require('dap').run_to_cursor() end)
setkey('tdt', function() require('dap').terminate() end)
setkey('tdn', function() require('dap-python').test_method() end)
setkey('tdc', function() require('dap-python').test_class() end)
setkey('tdl', function() require('dap').run_last() end)
-- setkey('<leader>e', function() require("dap.ui.widgets").hover() end)

setkey('tdw', ':DapViewToggle<cr>')
-- stepping (F-keys, standard debugger convention)
setkey('<F9>', function() require('dap').continue() end)
setkey('<F8>', function() require('dap').step_over() end)
setkey('<F7>', function() require('dap').step_into() end)
setkey('<S-F7>', function() require('dap').step_out() end)

-- native LSP completion (nvim 0.12+)
if vim.fn.has('nvim-0.12') == 1 then
    setkey('<c-space>', function()
        vim.lsp.completion.get()
    end, { 'i' })
end

vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(ev)
        local client = vim.lsp.get_client_by_id(ev.data.client_id)
        if
            vim.fn.has('nvim-0.12') == 1
            and native_completion_enabled(ev.buf)
            and client
            and client:supports_method('textDocument/completion')
        then
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

        -- require my custom priorised diagnostic command
        require('config.prioritised_diagnostic').setup()
    end,
})

vim.keymap.set("n", "<leader>ej", "<cmd>TeamtypeJumpToCursor<cr>")
vim.keymap.set("n", "<leader>ef", "<cmd>TeamtypeFollow<cr>")

local init_done_event_name = 'InitDone'

-- deferred initialization
vim.api.nvim_create_autocmd('User', {
    pattern = init_done_event_name,
    callback = function()
        vim.diagnostic.config({
            virtual_text = false,
            signs = true,
            underline = true,
        })
        vim.lsp.inlay_hint.enable(false)

        -- TODO: make this on file open/read
        -- require('modules/runtests').setup()

        require("ask-agent").setup({
            provider = "claude",
        })

        vim.o.statusline = vim.o.statusline .. " %{v:lua.ask_agent_statusline()}"

        -- configure neovide
        if vim.g.neovide then
            vim.g.neovide_cursor_animation_length = 0
        end
    end,

})

ask_agent_statusline = function()
    return require("ask-agent").statusline()
end


load_theme()

vim.o.cmdheight = 1

-- finally emit the config loaded event for deferred initialization
vim.schedule(function()
    vim.api.nvim_exec_autocmds('User', { pattern = init_done_event_name, modeline = false })
end)
