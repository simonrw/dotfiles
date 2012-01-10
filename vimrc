" Nice pathogen stuff
filetype off
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

" Set no vi compatible
set nocompatible

" set up some nice tab defaults
set tabstop=4
set shiftwidth=4
set smarttab
set expandtab

" reload all file changes automatically
set autoread

" automatically change directory to where the file is
set autochdir

" change leader key
let mapleader=","

" enable the semicolon key to act like the colon key
noremap ; :

" set incremental search
set incsearch

" set case options for searching
set ignorecase
set smartcase

" show command as it's typed
set showcmd

set background=dark

" nice file formatting
if has("autocmd")
  filetype plugin indent on
endif 
"
" line numbers
set number

" always have status line on
set laststatus=2


" Set colour scheme
if &t_Co >= 256 || has("gui_running")
	colorscheme lucius
	"colorscheme jellybeans
endif

" turn syntax highlighting on
if &t_Co > 2 || has("gui_running")
    syntax on
endif

" make backspace behave nicely
set backspace=indent,eol,start

" pyclewn port
"let g:pyclewn_connection="localhost:80801:changeme"

" if mac use the monaco font
if has("macunix")
    set gfn=Monaco:h12
endif

set mouse=a

" Vim latex commands
"set grepprg=grep\ -nH\ $*
"let g:Tex_CompileRule_dvi='latex -interaction=nonstopmode --src-specials $*'
"let g:tex_flavor='latex'

" Auto-clean fugitive buffers
autocmd BufReadPost fugitive://* set bufhidden=delete

" Set status line
set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P

" Remap the move-window keys
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
