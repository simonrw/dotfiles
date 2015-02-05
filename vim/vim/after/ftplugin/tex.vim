setlocal iskeyword+=:
setlocal sw=2
setlocal nowrap
setlocal tw=80
setlocal formatoptions+=tw
setlocal formatprg=
setlocal nolist
setlocal cc=0
setlocal nocursorcolumn
setlocal conceallevel=0
syntax spell toplevel

let g:tex_fold_enabled = 0

" Example synctex command, for skim
" map <silent> <Leader>ls :!~/Applications/Skim.app/Contents/SharedSupport/displayline -b -g <C-R>=line('.')<CR> "pdf-filename" "%:p" <CR>

let SectionRegex = '\v\\(sub)*section'
nmap [r ?<C-R>=SectionRegex<cr><cr>
nmap ]r /<C-R>=SectionRegex<cr><cr>
vmap [r ?<C-R>=SectionRegex<cr><cr>
vmap ]r /<C-R>=SectionRegex<cr><cr>
