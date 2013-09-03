map <leader>f :CtrlP<cr>
map <leader>r :call RunNearestSpec()<cr>

let g:rspec_command = '!bundle exec rspec {spec}'

" Snipmate settings - use Ctrl-J for completion
imap <c-j> <Plug>snipMateNextOrTrigger
smap <c-j> <Plug>snipMateNextOrTrigger
