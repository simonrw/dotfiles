if has('nvim')
    nnoremap <leader>y :update\|:RustFmt<Cr>
    nnoremap <leader>t :update\|:term cargo build<Cr>
    nnoremap <leader>r :update\|:term cargo run<Cr>
    nnoremap <leader>x :update\|:term cargo check<Cr>
else
    nnoremap <leader>y :update\|:RustFmt<Cr>
    nnoremap <leader>t :update\|:!cargo build<Cr>
    nnoremap <leader>r :update\|:!cargo run<Cr>
    nnoremap <leader>x :update\|:!cargo check<Cr>
endif
