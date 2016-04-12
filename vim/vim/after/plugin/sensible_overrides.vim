" vim-sensible overrides

" Update current completion settings
" use spelling if 'spell'
set complete+=kspell

" remove the ruler
set noruler

" neovim handles ESC keys as alt+key, set this to solve the problem
if has('nvim')
    set ttimeout
    set ttimeoutlen=0
else
    " Set the shell
    set shell=/bin/sh
endif

" Set the minimum window width to 79
set winwidth=79
