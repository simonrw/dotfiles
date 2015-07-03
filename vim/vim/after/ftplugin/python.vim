setlocal textwidth=79

setlocal wrap

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

" Make the colour column a nicer colour
highlight ColorColumn ctermbg=0
