function _G.nnoremap(lhs, rhs) vim.api.nvim_set_keymap('n', lhs, rhs, { noremap = true, silent = true }) end
function _G.cnoremap(lhs, rhs) vim.api.nvim_set_keymap('c', lhs, rhs, { noremap = true , silent = true}) end
function _G.inoremap(lhs, rhs) vim.api.nvim_set_keymap('i', lhs, rhs, { noremap = true , silent = true}) end
function _G.vnoremap(lhs, rhs) vim.api.nvim_set_keymap('v', lhs, rhs, { noremap = true , silent = true}) end

-- Very magic search always
nnoremap('/', '/\\v')
nnoremap('?', '?\\v')

-- split navigation
nnoremap('<C-h>', '<C-w><C-h>')
nnoremap('<C-j>', '<C-w><C-j>')
nnoremap('<C-k>', '<C-w><C-k>')
nnoremap('<C-l>', '<C-w><C-l>')

nnoremap('cp', ':0,$y+<Cr>')
nnoremap('\'', '`')
nnoremap('`', '\'')
nnoremap('j', 'gj')
nnoremap('k', 'gk')

cnoremap('%s/', '%sm/')

-- escape with Ctrl-j
inoremap('<C-j>', '<esc>')
vnoremap('<C-j>', '<esc>')
inoremap('<C-c>', '<esc>')
vnoremap('<C-c>', '<esc>')

-- Reselect visual block after indent/outdent
vnoremap('<', '<gv')
vnoremap('>', '>gv')

-- Expand %% to the current directory
cnoremap('%%', "<c-r>=expand('%:h') . '/'<cr>")

-- nmap <silent> <leader>l :call ToggleList("Location List", 'l')<CR>
nnoremap('Q', ":call ToggleList('Quickfix List', 'c')<CR>")

nnoremap('<leader>vr', ':source $MYVIMRC\\|echo "Reloaded!"<Cr>')
nnoremap('<leader>ev', ':echo "Use <leader>ve"<Cr>')
nnoremap('<Space>', ':nohl<cr>')

-- Bind K to grep word under cursor
nnoremap('K', ':grep! "<C-R><C-W>"<cr><cr>\\|:copen<Cr>')

