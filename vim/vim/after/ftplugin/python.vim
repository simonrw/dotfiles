setlocal textwidth=79

" Python autocompletion !
setlocal omnifunc=pythoncomplete#Complete

" Disable spelling
setlocal nospell

" Set the vim-pipe command
let b:vimpipe_command="python"

" Add the colourcolumn
setlocal colorcolumn=80

function! AddImportString(import_text)
    execute "normal! mmgg/import\<cr>}"
    execute "normal! O" . a:import_text . "\<esc>"
    execute "normal! 'm"
    execute "nohl"
    execute "normal! zz"
endfunction

function! AddImport()
    let import_text = input("import string: ")
    call AddImportString(import_text)
endfunction

nnoremap cii :call AddImport()<Cr>
nnoremap cin :call AddImportString("import numpy as np")<Cr>
nnoremap cim :call AddImportString("import matplotlib.pyplot as plt")<cr>
nnoremap cia :call AddImportString("from astropy.io import fits")<cr>
nnoremap cis :call AddImportString("import seaborn as sns")<cr>

" Allow linting with make plugin
setlocal makeprg=pylint\ --reports=n\ --output-format=parseable\ %:p
setlocal errorformat=%f:%l:\ %m
nnoremap <leader>x :update\|:Make<cr>

" Make the colour column a nicer colour
highlight ColorColumn ctermbg=0

" Run autopep8 for python files
nnoremap <buffer> <leader>y mm\|:%!autopep8 -<cr>\|'m

" Send lines to vtr pane
vmap <leader>t :VtrSendLines<cr>
