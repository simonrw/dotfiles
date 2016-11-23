" Enable c++11 formatting
setlocal syntax=cpp11

map <silent> <leader>y mm:0,$!clang-format<cr>'m

" Re-indent the whole buffer.
function! Indent()
  call Preserve('0,$!clang-format')
endfunction

if executable('clang-format')
    autocmd BufWritePre <buffer> silent call Indent()
endif
