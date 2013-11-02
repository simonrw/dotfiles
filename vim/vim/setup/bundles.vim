" Global setup
" -------------
" Set no vi compatible
set nocompatible

" Nice vundle stuff
filetype off

let g:vundle_default_git_proto = 'git'

" Handle vundle here
set rtp+=~/.vim/bundle/vundle
call vundle#rc()


" Let vundle manage vundle
Bundle 'gmarik/vundle'

" tpope plugins, these deserve their own section
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-dispatch'

" Custom plugins
Bundle 'kien/ctrlp.vim'
Bundle 'mileszs/ack.vim'
Bundle 'jnwhiteh/vim-golang'
Bundle 'kien/rainbow_parentheses.vim'

" My plugins or forks
Bundle 'mindriot101/vim-scratch'
Bundle 'mindriot101/srw-colorscheme.vim'

" Snipmate plugins
Bundle "MarcWeber/vim-addon-mw-utils"
Bundle "tomtom/tlib_vim"
Bundle 'garbas/vim-snipmate'
Bundle 'honza/vim-snippets'

" Language plugins
Bundle 'rodjek/vim-puppet'
Bundle 'jimenezrick/vimerl'

filetype plugin indent on
