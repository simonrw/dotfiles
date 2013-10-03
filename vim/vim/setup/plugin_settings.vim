let g:ctrlp_map = '<leader>f'
let g:ctrlp_cmd = 'CtrlPMixed'

" Snipmate settings - use Ctrl-J for completion
imap <c-j> <Plug>snipMateNextOrTrigger
smap <c-j> <Plug>snipMateNextOrTrigger

" Mapping for quick switching buffers
nnoremap <leader>b :CtrlPBuffer<cr>

" Fugitive mappings
nmap <leader>gc :Gcommit<cr>
nmap <leader>gd :Gdiff<cr>
nmap <leader>gsd :Gsdiff<cr>
nmap <leader>gw :Gwrite<cr>
nmap <leader>gr :Gread<cr>
nmap <leader>gl :Glog<cr>
