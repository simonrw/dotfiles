if has('nvim')
    nnoremap <leader>y :update\|:RustFmt<Cr>
    nnoremap <leader>t :update\|:T cargo build<Cr>
    nnoremap <leader>r :update\|:T cargo run<Cr>
    nnoremap <leader>x :update\|:T cargo check<Cr>

    autocmd! BufWritePost *.rs Neomake! cargo
else
    nnoremap <leader>y :update\|:RustFmt<Cr>
    nnoremap <leader>t :update\|:!cargo build<Cr>
    nnoremap <leader>r :update\|:!cargo run<Cr>
    nnoremap <leader>x :update\|:!cargo check<Cr>
endif
