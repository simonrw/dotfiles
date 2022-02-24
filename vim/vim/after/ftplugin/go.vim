let g:go_fmt_command = "goimports"
let g:go_template_autocreate = 0
setlocal noexpandtab
setlocal tabstop=4
setlocal shiftwidth=0

" nnoremap <silent> <leader>y :silent !goimports -w %<cr>
let b:ale_fixers = ["goimports"]
let b:ale_fix_on_save = 1
let b:ale_linters = ["gopls", 'gofmt', 'golint', 'govet']
