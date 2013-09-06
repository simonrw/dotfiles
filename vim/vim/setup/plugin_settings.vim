let g:ctrlp_map = '<leader>f'
let g:ctrlp_cmd = 'CtrlPMixed'
map <leader>r :call RunNearestSpec()<cr>

let g:rspec_command = '!rspec-runner {spec}'

" Snipmate settings - use Ctrl-J for completion
imap <c-j> <Plug>snipMateNextOrTrigger
smap <c-j> <Plug>snipMateNextOrTrigger
