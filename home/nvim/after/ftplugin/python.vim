let @i='import IPython; IPython.embed(); exit()'
let @p='import pudb; pudb.set_trace()'
let @n='if __name__ == "__main__":'

vnoremap <silent> <leader>t :VtrSendLinesToRunner<Cr>
nnoremap <silent> <leader>p vip:VtrSendLinesToRunner<Cr>

let b:ale_fixers = ["black"]
let b:ale_linters = ["pyright", "flake8"]
let g:black#settings = {
    \ 'line_length': 100
    \}

nnoremap <silent> <leader>y <cmd>call Black()<cr>

" override the pytest executable as pytest tries to be too clever when a
" Pipfile exists
let test#python#pytest#executable = 'pytest'

set textwidth=0
