setlocal iskeyword+=:
setlocal sw=2
setlocal nolist
setlocal cc=0
setlocal nocursorcolumn
setlocal conceallevel=0
syntax spell toplevel

" Default to running makefile
nnoremap <leader>t :update\|:!make<cr>
