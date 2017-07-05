" Save this into register `i` for easy inserting
let @i = 'import IPython; IPython.embed(); exit(1)'
let @p = 'import pdb; pdb.set_trace()'

set colorcolumn=81

" Set up ALE
let g:ale_linters = {
    \ 'python': ['flake8']
    \}

" nnoremap <silent> <cr> :call HandleEnter("\|T %")<cr>
