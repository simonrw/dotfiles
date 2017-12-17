" Based on

runtime colors/grb256.vim

let g:colors_name = "srw256"

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

" Make the colour column a little bit more subtle
" Dark background: 237
" Light background: 15
highlight ColorColumn ctermbg=237 guibg=#494949

" Make the search result a little less jarring
highlight Search guibg=NONE gui=underline guifg=NONE

" Highlight trailing whitespace
highlight ExtraWhitespace ctermbg=237 guibg=#333333
autocmd Syntax * syn match ExtraWhitespace /\s\+$/

" Handle ALE highlighting
hi SpellBad  term=reverse ctermfg=15 ctermbg=0 gui=bold guifg=white guibg=#FF6C60 guisp=Red
hi SpellCap  term=reverse ctermfg=15 ctermbg=0 gui=undercurl guisp=Blue
hi Error     term=reverse ctermfg=15 ctermbg=0 gui=undercurl guisp=#FF6C60
