let @i='import IPython; IPython.embed(); exit()'
let @p='import pudb; pudb.set_trace()'
let @n='if __name__ == "__main__":'
set nowrap
set textwidth=79

" Set up auto formatting
if executable('black')
    nnoremap <leader>y mm:%!black --quiet --fast -<Cr>`m
    vnoremap <leader>y :'<,'>!black --quiet --fast -<Cr>
endif

vnoremap <silent> <leader>t :VtrSendLinesToRunner<Cr>
nnoremap <silent> <leader>p vip:VtrSendLinesToRunner<Cr>

setlocal colorcolumn=80
