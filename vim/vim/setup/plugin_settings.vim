let g:ctrlp_map = '<leader>f'
let g:ctrlp_cmd = 'CtrlPMixed'
map <leader>r :w\|call RunNearestSpec()<cr>

let g:rspec_command = '!rspec-runner {spec}'

" Snipmate settings - use Ctrl-J for completion
imap <c-j> <Plug>snipMateNextOrTrigger
smap <c-j> <Plug>snipMateNextOrTrigger

" Taglist
map <leader><tab> :TagbarToggle<cr>

" Fugitive mappings
nmap <leader>gc :Gcommit<cr>
nmap <leader>gd :Gdiff<cr>
nmap <leader>gsd :Gsdiff<cr>
nmap <leader>gw :Gwrite<cr>
nmap <leader>gr :Gread<cr>
nmap <leader>gl :Glog<cr>


" If ag is available then use it
if executable("ag")
    let g:ackprg = 'ag --nogroup --nocolor --column'
endif
