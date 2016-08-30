" Based on
runtime colors/solarized.vim

" Make the colour column a little bit more subtle
" Dark background: 237
" Light background: 15
highlight ColorColumn ctermbg=237

" Make rust documtnation a little more muted
highlight rustCommentLineDoc ctermfg=137

" Custom background colour
hi Normal guibg=#282828
hi NonText guibg=#282828

" Highlight trailing whitespace
highlight ExtraWhitespace ctermbg=237 guibg=#333333
autocmd Syntax * syn match ExtraWhitespace /\s\+$/
