let @i='import ipdb; ipdb.set_trace()'
set textwidth=80
set nowrap

" Set up auto formatting
if executable('black')
    nnoremap <leader>y mm:%!black --fast -q -<Cr>`m
endif

setlocal cc=+1
