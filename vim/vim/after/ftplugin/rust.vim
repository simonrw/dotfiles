nnoremap <leader>y :update\|:RustFmt<Cr>
if has('nvim') && $TMUX == ''
    nnoremap <leader>t :update\|:T cargo build<Cr>
    nnoremap <leader>r :update\|:T cargo run<Cr>
    nnoremap <leader>x :update\|:T cargo check<Cr>
elseif $TMUX != ''
    nnoremap <leader>t :update\|:V cargo build<Cr>
    nnoremap <leader>r :update\|:V cargo run<Cr>
    nnoremap <leader>x :update\|:V cargo check<Cr>
else
    nnoremap <leader>t :update\|:!cargo build<Cr>
    nnoremap <leader>r :update\|:!cargo run<Cr>
    nnoremap <leader>x :update\|:!cargo check<Cr>
endif
autocmd! BufWritePost *.rs Neomake! cargo
