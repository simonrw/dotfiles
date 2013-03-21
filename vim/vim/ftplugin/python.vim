if v:version >= 703
    setlocal colorcolumn=80
endif

" Change the saving behaviour, strip empty whitespace
au BufWritePre <buffer> :%s/\s\+$//e

setlocal tabstop=4
setlocal shiftwidth=4
setlocal expandtab
setlocal smarttab
setlocal formatoptions=croql
setlocal softtabstop=4

