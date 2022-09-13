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
    let g:ale_echo_cursor = 1
    let g:ale_set_balloons = 0
    let g:ale_hover_cursor = 0
    let g:ale_completion_enabled = 1
    let g:ale_set_highlights = 0

    set omnifunc=ale#completion#OmniFunc

    let g:ale_fix_on_save = 1
endif
