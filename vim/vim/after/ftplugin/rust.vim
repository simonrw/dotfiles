augroup rust_neomake
    autocmd!
    autocmd! BufWritePost *.rs Neomake! cargo
augroup END
