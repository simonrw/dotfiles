" function! Formatonsave()
"   let l:formatdiff = 1
"   :silent !clang-format -i $(find src -name '*.cpp') $(find include -name '*.h')
" endfunction
" autocmd BufWritePre *.h,*.cc,*.cpp call Formatonsave()
