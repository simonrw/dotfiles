let @i='import IPython; IPython.embed(); exit()'
let @p='import pudb; pudb.set_trace()'
let @n='if __name__ == "__main__":'
set nowrap
set textwidth=79

" Set up auto formatting
if executable('autopep8')
    nnoremap <leader>y mm:%!autopep8 -<Cr>`m
    vnoremap <leader>y :'<,'>!autopep8 -<Cr>
endif

vnoremap <silent> <leader>t :VtrSendLinesToRunner<Cr>
nnoremap <silent> <leader>p vip:VtrSendLinesToRunner<Cr>

setlocal colorcolumn=80
