if executable('clang-format')
    autocmd BufWritePre <buffer> silent call ClangFormat()
endif

