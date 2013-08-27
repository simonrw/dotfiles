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

" Plugins
" --------

" these 2 are required for snipmate plugin
Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'tomtom/tlib_vim'

Bundle 'altercation/vim-colors-solarized'
Bundle 'garbas/vim-snipmate'
Bundle 'honza/vim-snippets'
Bundle 'kchmck/vim-coffee-script'
Bundle 'LaTeX-Box-Team/LaTeX-Box'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'Shougo/unite-outline'
Bundle 'Shougo/unite.vim'
Bundle 'Shougo/vimproc.vim'
Bundle 'thoughtbot/vim-rspec'
Bundle 'tomtom/tcomment_vim'
Bundle 'tpope/vim-dispatch'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-unimpaired'

filetype plugin indent on
