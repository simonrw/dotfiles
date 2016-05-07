" Enable the colour column
setlocal colorcolumn=80


" Disable spelling
setlocal nospell

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
nnoremap cis :call AddImportString("import sys")<cr>
nnoremap cio :call AddImportString("import os")<cr>
nnoremap cif :call AddImportString("import fitsio")<cr>

" Allow linting with make plugin
setlocal makeprg=pylint\ --reports=n\ --output-format=parseable\ %:p
setlocal errorformat=%f:%l:\ %m
nnoremap <leader>x :update\|:Make<cr>

" Run autopep8 for python files
nnoremap <buffer> <leader>y mm\|:%!autopep8 --max-line-length 90 -<cr>\|'m

" Send lines to vtr pane
vmap <leader>t :VtrSendLines<cr>

" Quick shortcuts for inserting common python lines
iabbrev #! #!/usr/bin/env python
\<CR># -*- coding: utf-8 -*-
iabbrev ipy import IPython; IPython.embed(); exit(1)
iabbrev ifmain if __name__ == '__main__':
iabbrev future from __future__ import absolute_import, division, print_function
