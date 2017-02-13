augroup rust_auto
    autocmd!
    autocmd! BufWritePost *.rs Neomake! cargo
    autocmd! BufWritePre *.rs Neoformat
augroup END

nnoremap <silent> K :call LanguageClient_textDocument_hover()<Cr>
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
