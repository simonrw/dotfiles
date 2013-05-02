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

" Set the makeprg to flake8 for syntax checking
setlocal makeprg=flake8\ %

" Set up some common spelling replacements
Abolish wiht with
Abolish improt import
