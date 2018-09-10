let @i='import ipdb; ipdb.set_trace()'
set textwidth=72
set nowrap

" Set up auto formatting
if executable('black')
    nnoremap <leader>y mm:%!black --fast -q -<Cr>`m
endif
