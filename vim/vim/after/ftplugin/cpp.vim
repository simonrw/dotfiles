" Enable c++11 formatting
setlocal syntax=cpp11

if executable('clang-format')
    autocmd BufWritePre <buffer> silent call ClangFormat()
endif
