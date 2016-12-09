" Enable c++11 formatting
setlocal syntax=cpp11

if executable('clang-format')
    nnoremap <leader>y :call ClangFormat()<Cr>
endif
