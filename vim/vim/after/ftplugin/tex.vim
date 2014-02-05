setlocal iskeyword+=:
setlocal sw=2
setlocal wrap
setlocal nolist
setlocal linebreak
setlocal cc=0
setlocal nocursorcolumn
setlocal conceallevel=0
setlocal re=2
syntax spell toplevel


" Example synctex command, for skim
" map <silent> <Leader>ls :!~/Applications/Skim.app/Contents/SharedSupport/displayline -b -g <C-R>=line('.')<CR> "pdf-filename" "%:p" <CR>

" Tag list settings for latex
let tlist_tex_settings = 'latex;l:labels;s:sections;t:subsections;u:subsubsections'
setlocal iskeyword=@,48-57,_,-,:,192-255

let SectionRegex = '\v\\(sub)*section'
nmap [r ?<C-R>=SectionRegex<cr><cr>
nmap ]r /<C-R>=SectionRegex<cr><cr>
