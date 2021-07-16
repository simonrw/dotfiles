vim.g.fzf_command_prefix = 'Fzf'
vim.g.fzf_preview_window = {}

nnoremap('<leader>f', ':FzfGitFiles<cr>')
nnoremap('<leader>F', ':FzfFiles<cr>')
nnoremap('gb', ':FzfBuffers<cr>')
nnoremap('gl', ':FzfRg<cr>')
nnoremap('gL', ':FzfBLines<cr>')
nnoremap('<leader>gT', ':FzfTags<cr>')
nnoremap('<leader>gt', ':FzfBTags<cr>')
nnoremap('<leader>a', ':FzfRg<cr>')


