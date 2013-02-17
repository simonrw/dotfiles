if v:version >= 703
    setlocal colorcolumn=80
endif

" Change the saving behaviour, strip empty whitespace
au BufWritePre <buffer> :%s/\s\+$//e

setlocal tabstop=2
setlocal shiftwidth=2
setlocal expandtab
setlocal smarttab
setlocal formatoptions=croql
setlocal softtabstop=2

