vim.opt.mouse = 'a'
vim.opt.scrolloff = 5
vim.opt.number = false
vim.opt.relativenumber = false
vim.opt.cursorline = false
vim.opt.ttimeoutlen = 10
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
vim.opt.smartcase = true
vim.opt.expandtab = true
vim.opt.ignorecase = true
vim.opt.hidden = true
vim.opt.tags:append('.git/tags')
vim.opt.wildmode = { 'list:longest', 'list:full' }
vim.opt.history = 50
vim.opt.complete = { '.', 'w', 'b', 'u', 't', 'i' }
vim.opt.completeopt = { 'menuone', 'preview' }
vim.opt.ruler = true
vim.opt.list = false
vim.opt.showcmd = true
vim.opt.incsearch = true
vim.opt.hlsearch = false
vim.opt.laststatus = 2
vim.opt.backspace = { 'indent', 'eol', 'start' }
vim.opt.autowrite = true
vim.opt.wrap = false
vim.opt.textwidth = 0
vim.opt.breakindent = true
vim.opt.synmaxcol = 1024
vim.opt.termguicolors = true
vim.opt.inccommand = 'nosplit'
vim.opt.linebreak = true
vim.opt.gdefault = true
-- Configure minimum window width
vim.opt.winwidth = 80
-- speed up UI updates
vim.opt.updatetime = 100
-- If buffer is already open when switching, switch to it
vim.opt.switchbuf:append('useopen')
-- configure concealing things
vim.opt.conceallevel = 0
-- configure shortmessages
vim.opt.shortmess = 'tToOFIWa'
-- -- Do not hide sign column
vim.opt.signcolumn = 'yes'

-- Sane splits
vim.opt.splitright = true
vim.opt.splitbelow = true

-- Set up backups
-- Protect changes between writes. Default values of
-- updatecount (200 keystrokes) and updatetime
-- (4 seconds) are fine
vim.opt.swapfile = true
vim.opt.directory:prepend('~/.vim/swap//')

-- protect against crash-during-write
vim.opt.writebackup = true
-- but do not persist backup after successful write
vim.opt.backup = false
-- use rename-and-write-new method whenever safe
vim.opt.backupcopy = 'auto'

-- consolidate the writebackups -- not a big
-- deal either way, since they usually get deleted
vim.opt.backupdir:prepend('~/.vim/backup//')

-- configure format options to wrap in commented blocks
vim.opt.formatoptions:append('ro')

-- Set up persistent undo
vim.opt.undofile = true

vim.opt.modeline = true
vim.opt.lazyredraw = true

vim.opt.background = 'dark'

