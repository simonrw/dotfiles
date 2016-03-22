" Highlight the self keyword
syntax keyword Keyword self

" Highlight long lines with subtle colour
highlight LongLineTail term=standout ctermfg=9 ctermbg=NONE gui=bold guifg=white guibg=#FF6C60
syntax match LongLineTail "\%>90v.\+"

