setlocal foldmethod=marker
setlocal iskeyword+=:
setlocal sw=2
setlocal wrap
setlocal nolist
setlocal spell
setlocal linebreak
setlocal nonumber
setlocal cc=0
setlocal nocursorcolumn

" Synctex in skim
map <silent> <LocalLeader>ls :silent !/Applications/Skim.app/Contents/SharedSupport/displayline <C-R>=line('.')<CR> "<C-R>=LatexBox_GetOutputFile()<CR>" "%:p" <CR>
