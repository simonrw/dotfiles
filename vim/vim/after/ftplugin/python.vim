let @i='import IPython; IPython.embed(); exit()'
let @p='import pudb; pudb.set_trace()'
let @n='if __name__ == "__main__":'
set nowrap
set textwidth=0

" Set up auto formatting
nnoremap <leader>y mm:Format<Cr>`m

vnoremap <silent> <leader>t :VtrSendLinesToRunner<Cr>
nnoremap <silent> <leader>p vip:VtrSendLinesToRunner<Cr>

let g:ale_fixers = ["black"]

setlocal colorcolumn=80

" override the pytest executable as pytest tries to be too clever when a
" Pipfile exists
let test#python#pytest#executable = 'pytest'
