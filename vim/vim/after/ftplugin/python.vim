let @i='import IPython; IPython.embed(); exit()'
let @p='import pudb; pudb.set_trace()'
let @n='if __name__ == "__main__":'

vnoremap <silent> <leader>t :VtrSendLinesToRunner<Cr>
nnoremap <silent> <leader>p vip:VtrSendLinesToRunner<Cr>

let b:ale_fixers = ["black"]
let b:ale_linters = ["pyright", "flake8"]

" override the pytest executable as pytest tries to be too clever when a
" Pipfile exists
let test#python#pytest#executable = 'pytest'

set textwidth=0

command! -nargs=* Mypy call python#run_mypy("--strict --ignore-missing-imports", expand("%"))
command! -nargs=* Flake8 call python#run_flake8(<f-args>)

nnoremap <silent> <leader>y mm:%!black -q -<cr>`m
