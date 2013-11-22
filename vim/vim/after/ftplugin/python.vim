setlocal textwidth=99

" Highlight the 81st column
call matchadd('ColorColumn', '\%81v', 100)

set wrap

" Set the makeprg to flake8 for syntax checking
setlocal makeprg=flake8\ %

" Python autocompletion !
setlocal omnifunc=pythoncomplete#Complete

" Disable spelling
setlocal nospell

" Set the flake8 settings
setlocal makeprg=flake8\ %\ --ignore=E501,E128,E123,E126,E261,E262
