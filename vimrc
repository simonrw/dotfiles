" Nice pathogen stuff
filetype off

" Handle disabling pathogen under certain circumstances
let g:pathogen_disabled = []

" If the clang binary is not available
let s:clang_search = system("which clang")
if !empty(matchstr(s:clang_search, "clang not found"))
    echo "Cannot find clang, disabling clang completion"
    call add(g:pathogen_disabled, "clang_complete")
endif


" Import the pathogen modules
call pathogen#infect()

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
set encoding=utf-8 " Necessary to show unicode glyphs

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

" Remove the arrow key functionality
noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>

" Set Ultisnips comment style
let g:ultisnips_python_style='doxygen'

" Set the tags directory
set tags=./tags;/
"map <F8> :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>

" Set tags executable
if has("macunix")
    " Have to change this for my laptop
    let Tlist_Ctags_Cmd='/usr/local/bin/ctags'
endif

" Shortcut for the tag list 
map <leader>t :TlistToggle<CR>

" Relative and absolute line number toggling
function! NumberToggle()
    if (&relativenumber == 1)
        set number
    else
        set relativenumber
    endif
endfunc


" Map this to a nice keyboard shortcut
nnoremap <leader>n :call NumberToggle()<CR>

" Set absolute mode whenever window loses focus
:au FocusLost * :set number

" Some clang completion settings

" Show clang errors in quickfix window
let g:clang_complete_copen = 1


" Automatically reload the vimrc file when saved
au BufWritePost .vimrc so ~/.vimrc

" Use sane regexes
nnoremap / /\v
vnoremap / /\v

" Reselect visual block after indent/outdent
vnoremap < <gv
vnoremap > >gv

" Set text width with tex files
au FileType tex set textwidth=72
au FileType tex set spell

" Change the way tab autocompletion works
set wildmode=list:longest,list:full
