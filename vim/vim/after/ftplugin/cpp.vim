augroup cpp_neomake
    autocmd!
    autocmd! BufWritePost *.cpp,*.h Neomake!
augroup END
