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

Bundle 'bjoernd/vim-weasel'
Bundle 'BufOnly.vim'
Bundle 'digitaltoad/vim-jade'
Bundle 'frerich/unicode-haskell'
Bundle 'garbas/vim-snipmate'
Bundle 'honza/vim-snippets'
Bundle 'jgdavey/tslime.vim'
Bundle 'kana/vim-altr'
Bundle 'kana/vim-niceblock'
Bundle 'kchmck/vim-coffee-script'
Bundle 'LargeFile'
Bundle 'LaTeX-Box-Team/LaTeX-Box'
Bundle 'Lokaltog/vim-distinguished'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'lukerandall/haskellmode-vim'
Bundle 'michaeljsmith/vim-indent-object'
Bundle 'mikewest/vimroom'
Bundle 'mileszs/ack.vim'
Bundle 'mindriot101/vim-tslime-input'
Bundle 'Shougo/unite-outline'
Bundle 'Shougo/unite.vim'
Bundle 'Shougo/vimproc.vim'
Bundle 'sjl/splice.vim'
Bundle 'thoughtbot/vim-rspec'
Bundle 'tomasr/molokai'
Bundle 'tomtom/tcomment_vim'
Bundle 'tpope/vim-abolish'
Bundle 'tpope/vim-bundler'
Bundle 'tpope/vim-dispatch'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-rake'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-unimpaired'
Bundle 'zeis/vim-kolor'
Bundle 'takac/vim-hardtime'
Bundle 'ctags.vim'

filetype plugin indent on
