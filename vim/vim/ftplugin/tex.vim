setlocal foldmethod=marker
setlocal iskeyword+=:
setlocal sw=2
setlocal wrap
setlocal nolist
setlocal spell
setlocal linebreak
setlocal nonumber

" Ignore trash files for file globbing
" taken from https://github.com/lukepfister/.dotfiles/blob/master/.vimrc
let g:ctrlp_custom_ignore = {
			\ 'file': '\v\.(ps|pdf|dvi|aux)$'}
