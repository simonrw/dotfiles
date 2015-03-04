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

" Set some custom expansions, basically snippets but without plugins
iabbrev #! #!/usr/bin/env python
iabbrev coding # -*- coding: utf-8 -*-
iabbrev inumpy import numpy as np
iabbrev iplt import matplotlib.pyplot as plt
iabbrev ifmain if __name__ == '__main__':
iabbrev future from __future__ import division, print_function, absolute_import
iabbrev ipyembed import IPython; IPython.embed(); exit()

function! AddImportString(import_text)
    execute "normal! mmgg/import\<cr>}"
    execute "normal! O" . a:import_text . "\<esc>"
    execute "normal! 'm"
    execute "nohl"
endfunction

function! AddImport()
    let import_text = input("import string: ")
    call AddImportString(import_text)
endfunction

nnoremap <leader>ii :call AddImport()<Cr>
nnoremap <leader>in :call AddImportString("import numpy as np")<Cr>
nnoremap <leader>im :call AddImportString("import matplotlib.pyplot as plt")<cr>
