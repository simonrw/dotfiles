setlocal foldmethod=marker
setlocal iskeyword+=:
setlocal sw=2
setlocal wrap
setlocal nolist
setlocal spell
setlocal linebreak
setlocal cc=0
setlocal nocursorcolumn
setlocal conceallevel=0
setlocal re=2
syntax spell toplevel


" Synctex in skim
if has("gui_running")
    map <silent> <Leader>ls :silent !/Applications/Skim.app/Contents/SharedSupport/displayline -b -g <C-R>=line('.')<CR> "Thesis.pdf" "%:p" <CR>
else
    map <silent> <Leader>ls :!/Applications/Skim.app/Contents/SharedSupport/displayline -b -g <C-R>=line('.')<CR> "Thesis.pdf" "%:p" <CR>
endif

" Tag list settings for latex
let tlist_tex_settings = 'latex;l:labels;s:sections;t:subsections;u:subsubsections'
set iskeyword=@,48-57,_,-,:,192-255
