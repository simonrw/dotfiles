" Disable wrapping
setlocal textwidth=0
set nowrap

" Disable spelling
setlocal nospell

" Set the vim-pipe command
let b:vimpipe_command="python"

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
nnoremap cis :call AddImportString("import sys")<cr>
nnoremap cio :call AddImportString("import os")<cr>

" Allow linting with make plugin
setlocal makeprg=pylint\ --reports=n\ --output-format=parseable\ %:p
setlocal errorformat=%f:%l:\ %m
nnoremap <leader>x :update\|:Make<cr>

" Run autopep8 for python files
nnoremap <buffer> <leader>y mm\|:%!autopep8 --max-line-length 90 -<cr>\|'m

" Send lines to vtr pane
vmap <leader>t :VtrSendLines<cr>
