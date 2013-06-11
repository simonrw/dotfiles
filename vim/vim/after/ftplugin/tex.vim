setlocal foldmethod=marker
setlocal iskeyword+=:
setlocal sw=2
setlocal wrap
setlocal nolist
setlocal spell
setlocal linebreak
setlocal cc=0
setlocal nocursorcolumn

" Synctex in skim
if has("gui_running")
    map <silent> <LocalLeader>ls :silent !/Applications/Skim.app/Contents/SharedSupport/displayline -b -g <C-R>=line('.')<CR> "<C-R>=LatexBox_GetOutputFile()<CR>" "%:p" <CR>
else
    map <silent> <LocalLeader>ls :!/Applications/Skim.app/Contents/SharedSupport/displayline -b -g <C-R>=line('.')<CR> "<C-R>=LatexBox_GetOutputFile()<CR>" "%:p" <CR>
endif
