if g:completion_framework == "ale"
    nmap <silent> [g <Plug>(ale_previous_wrap)
    nmap <silent> ]g <Plug>(ale_next_wrap)
    nnoremap <silent> <leader>y :ALEFix<cr>
    nnoremap <silent> gd :ALEGoToDefinition<Cr>
    nnoremap <silent> gh :ALEFindReferences<cr>
    nnoremap <silent> <leader>ad :ALEDetail<Cr>

    " set up signs
    let g:ale_sign_error = ">"
    let g:ale_sign_warning = "~"
    let g:ale_virtualtext_cursor = 1
    let g:ale_cursor_detail = 0
    let g:ale_echo_cursor = 0
    let g:ale_set_balloons = 0
    let g:ale_hover_cursor = 0
    let g:ale_completion_enabled = 0
    let g:ale_set_highlights = 0

    set omnifunc=ale#completion#OmniFunc

    function! ALELinterStatus() abort
        let l:counts = ale#statusline#Count(bufnr(''))

        let l:all_errors = l:counts.error + l:counts.style_error
        let l:all_non_errors = l:counts.total - l:all_errors

        return l:counts.total == 0 ? 'OK' : printf(
        \   '%dW %dE',
        \   all_non_errors,
        \   all_errors
        \)
    endfunction

    set statusline=%<%f\ (%{ALELinterStatus()})\ %h%m%r%=%-14.(%l,%c%V%)\ %P
    let g:ale_fix_on_save = 1
endif
