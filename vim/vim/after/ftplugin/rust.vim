augroup rust_auto
    autocmd!
    autocmd! BufWritePost *.rs Neomake! cargo
    autocmd! BufWritePre *.rs Neoformat
augroup END
