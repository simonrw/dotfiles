" Enable c++11 formatting
setlocal syntax=cpp11

if executable('clang-format')
    nnoremap <leader>y :call ClangFormat()<Cr>
endif

if has('nvim') && $TMUX == ''
    nnoremap <leader>t :update\|:T make<Cr>
    nnoremap <leader>r :update\|:T make run<Cr>
elseif $TMUX != ''
    nnoremap <leader>t :update\|:V make<Cr>
    nnoremap <leader>r :update\|:V make run<Cr>
else
    nnoremap <leader>t :update\|:!make<Cr>
    nnoremap <leader>r :update\|:!make run<Cr>
endif
