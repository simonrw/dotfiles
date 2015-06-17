setlocal iskeyword+=:
setlocal sw=2
setlocal nolist
setlocal cc=0
setlocal nocursorcolumn
setlocal conceallevel=0
syntax spell toplevel

let SectionRegex = '\v\\(sub)*section'
nmap [r ?<C-R>=SectionRegex<cr><cr>
nmap ]r /<C-R>=SectionRegex<cr><cr>
vmap [r ?<C-R>=SectionRegex<cr><cr>
vmap ]r /<C-R>=SectionRegex<cr><cr>

" Default to running makefile
nnoremap <leader>t :update\|:!make<cr>
