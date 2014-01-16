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

nmap <leader>r :CtrlPMRU<cr>

" Use silver searcher if available
if executable('ag')
    " Let ctrlp use ag for files
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

    " Disable ag caching
    let g:ctrlp_use_caching = 0
endif

"}}}
" {{{ ack
let g:ackprg = 'ag --nogroup --nocolor --column'
nnoremap <leader>a :Ack!<space>
"}}}
" {{{ markdown-folding
let g:markdown_fold_style = 'stacked'
"}}}
" vim: foldmethod=marker
