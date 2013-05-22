if v:version >= 703
    setlocal colorcolumn=80
endif

setlocal formatoptions=croql

" Set the makeprg to flake8 for syntax checking
setlocal makeprg=flake8\ %
