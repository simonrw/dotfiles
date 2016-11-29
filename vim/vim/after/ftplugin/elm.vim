nnoremap <silent> <leader>y :update\|:call Preserve('%!elm-format --stdin')<cr>

autocmd BufWritePost *.elm silent call Preserve('%!elm-format --stdin')
