Plug 'dense-analysis/ale'

" ALE
nmap <silent> [g <Plug>(ale_previous_wrap)
nmap <silent> ]g <Plug>(ale_next_wrap)
nnoremap <silent> <leader>y <Plug>(ale_fix)<cr>

function! LinterStatus() abort
    let l:counts = ale#statusline#Count(bufnr(''))

    let l:all_errors = l:counts.error + l:counts.style_error
    let l:all_non_errors = l:counts.total - l:all_errors

    return l:counts.total == 0 ? 'OK' : printf(
    \   '%dW %dE',
    \   all_non_errors,
    \   all_errors
    \)
endfunction

set statusline=%<%f\ (%{LinterStatus()})\ %h%m%r%=%-14.(%l,%c%V%)\ %P
