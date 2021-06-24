" function! Formatonsave()
"   let l:formatdiff = 1
"   :silent !clang-format -i $(find src -name '*.cpp') $(find include -name '*.h')
" endfunction
" autocmd BufWritePre *.h,*.cc,*.cpp call Formatonsave()
nnoremap <silent> <leader>y mm:%!clang-format<cr>'m
let b:ale_linters = ["clangd"]

setlocal shiftwidth=2
setlocal tabstop=2
setlocal expandtab
