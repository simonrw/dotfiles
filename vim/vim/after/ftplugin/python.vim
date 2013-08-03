setlocal textwidth=99

if v:version >= 703
    setlocal colorcolumn=+1
endif

set wrap

setlocal formatoptions=croql

" Set the makeprg to flake8 for syntax checking
setlocal makeprg=flake8\ %

" Python autocompletion !
setlocal omnifunc=pythoncomplete#Complete 

" Disable spelling
setlocal nospell
