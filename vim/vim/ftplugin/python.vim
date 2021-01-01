" Settings here can be overwridden
setlocal nowrap
setlocal textwidth=0

" Set up auto formatting
if executable('black')
    nnoremap <leader>y mm:%!black --fast -q -<Cr>`m
    vnoremap <leader>y :'<,'>!black --fast -q -<Cr>
endif

