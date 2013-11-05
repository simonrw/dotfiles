" Enable matchit
runtime macros/matchit.vim

" Snipmate settings - use Ctrl-J for completion
imap <c-j> <Plug>snipMateNextOrTrigger
smap <c-j> <Plug>snipMateNextOrTrigger

" Fugitive mappings
nmap <leader>gc :Gcommit<cr>
nmap <leader>gd :Gdiff<cr>
nmap <leader>gsd :Gsdiff<cr>
nmap <leader>gw :Gwrite<cr>
nmap <leader>gr :Gread<cr>
nmap <leader>gl :Glog<cr>

" Ctrl-p settings
let g:ctrlp_map = "<leader>f"
let g:ctrlp_cmd = "CtrlPMixed"

" Set colour scheme
set background=light
colorscheme solarized
call togglebg#map("<leader>b")

" Rainbow parentheses setup
let g:rbpt_colorpairs = [
    \ ['brown',       'RoyalBlue3'],
    \ ['Darkblue',    'SeaGreen3'],
    \ ['darkgray',    'DarkOrchid3'],
    \ ['darkgreen',   'firebrick3'],
    \ ['darkcyan',    'RoyalBlue3'],
    \ ['darkred',     'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['brown',       'firebrick3'],
    \ ['gray',        'RoyalBlue3'],
    \ ['black',       'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['Darkblue',    'firebrick3'],
    \ ['darkgreen',   'RoyalBlue3'],
    \ ['darkcyan',    'SeaGreen3'],
    \ ['darkred',     'DarkOrchid3'],
    \ ['red',         'firebrick3'],
    \ ]

let g:rbpt_max = 16
let g:rbpt_loadcmd_toggle = 0

au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

" Ack.vim
nnoremap <leader>a :Ack!<space>

" markdown-folding
let g:markdown_fold_style = 'stacked'
