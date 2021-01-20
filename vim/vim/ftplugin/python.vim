" Settings here can be overwridden
setlocal nowrap
setlocal textwidth=0

" bindings for running custom commands
nnoremap <silent> <leader>m :Mypy<cr>

function! s:list_functions_in_buffer() abort
    silent grep 'def ' %
endfunction

function! s:list_test_functions_in_buffer() abort
    silent grep 'def test_' %
endfunction

command! PyListTestFunctions :call <SID>list_test_functions_in_buffer()
command! PyListFunctions call <SID>list_functions_in_buffer()
