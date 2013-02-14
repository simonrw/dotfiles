if v:version >= 703
    setlocal colorcolumn=80
endif

" Change the saving behaviour, strip empty whitespace
au BufWritePre <buffer> :%s/\s\+$//e

