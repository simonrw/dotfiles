if v:version >= 703
    setlocal colorcolumn=80
endif

setlocal tabstop=4
setlocal shiftwidth=4
setlocal expandtab
setlocal smarttab
setlocal formatoptions=croql
setlocal softtabstop=4

" Set the makeprg to flake8 for syntax checking
setlocal makeprg=flake8\ %
