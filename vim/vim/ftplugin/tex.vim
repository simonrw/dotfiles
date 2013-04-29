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

" Ignore trash files for file globbing
" taken from https://github.com/lukepfister/.dotfiles/blob/master/.vimrc
let g:ctrlp_custom_ignore = {
			\ 'file': '\v\.(ps|pdf|dvi|aux)$'}

" Synctex in skim
map <silent> <LocalLeader>ls :silent !/Applications/Skim.app/Contents/SharedSupport/displayline <C-R>=line('.')<CR> "<C-R>=LatexBox_GetOutputFile()<CR>" "%:p" <CR>
