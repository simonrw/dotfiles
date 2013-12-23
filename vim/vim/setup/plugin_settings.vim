" {{{ matchit
runtime macros/matchit.vim
"}}}
" {{{ snipmate
imap <c-j> <Plug>snipMateNextOrTrigger
smap <c-j> <Plug>snipMateNextOrTrigger
"}}}
" {{{ fugitive
nmap <leader>gc :Gcommit<cr>
nmap <leader>gd :Gdiff<cr>
nmap <leader>gw :Gwrite<cr>
nmap <leader>gr :Gread<cr>
nmap <leader>gl :Glog<cr>
"}}}
" {{{ ctrlp
let g:ctrlp_map = "<leader>f"

" Aliases for rails projects
nmap <leader>gm :CtrlP app/models<cr>
nmap <leader>gv :CtrlP app/views<cr>
nmap <leader>gc :CtrlP app/controllers<cr>
nmap <leader>ga :CtrlP app/assets<cr>
nmap <leader>gl :CtrlP lib<cr>
nmap <leader>gM :CtrlP db/migrate<cr>
nmap <leader>gs :CtrlP spec<cr>

nmap <leader>r :CtrlPMRU<cr>

" Use silver searcher if available
if executable('ag')
    " Let ctrlp use ag for files
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

    " Disable ag caching
    let g:ctrlp_use_caching = 0
endif

"}}}
" {{{ rainbow_parentheses
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
"}}}
" {{{ ack
let g:ackprg = 'ag --nogroup --nocolor --column'
nnoremap <leader>a :Ack!<space>
"}}}
" {{{ markdown-folding
let g:markdown_fold_style = 'stacked'
"}}}
" {{{ vimux
map <leader>vp :VimuxPromptCommand<cr>
map <leader>vl :VimuxRunLastCommand<cr>
map <leader>vi :VimuxInspectRunner<cr>
"}}}
" {{{ syntastic
" set passive mode by default
let g:syntastic_mode_map = { 'mode': 'passive' }
let g:syntastic_check_on_wq = 0
let g:syntastic_enable_highlighting = 0
map <leader>s :w\|SyntasticCheck<cr>
"}}}
" {{{ vim-rspec
map <leader>Rt :call RunCurrentSpecFile()<cr>
map <leader>Rs :call RunNearestSpec()<cr>
map <leader>Rl :call RunLastSpec()<cr>
map <leader>Ra :call RunAllSpecs()<cr>
"}}}
" vim: foldmethod=marker
