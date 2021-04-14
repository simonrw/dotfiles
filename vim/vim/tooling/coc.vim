Plug 'neoclide/coc.nvim', { 'branch': 'release' }
Plug 'rodrigore/coc-tailwind-intellisense', {'do': 'npm install'}

" configure conflicted status line
set statusline=%<%f\ %{coc#status()}%{get(b:,'coc_current_function','')}%h%m%r%=%-14.(%l,%c%V%)\ %P
set statusline+=%{ConflictedVersion()}

" Triggering completion
inoremap <silent><expr> <c-p> coc#refresh()
inoremap <silent><expr> <c-n> coc#refresh()

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nnoremap <silent> gh :CocList symbols<cr>
nnoremap <silent> <leader>cr :CocRestart<cr>

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')
nnoremap <silent> <leader>y :Format<cr>

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Use K to show documentation in preview window.
nnoremap <silent> <leader>d :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Prevent missing cursor bug
let g:coc_disable_transparent_cursor = 1

" prevent coc-emoji leaving behind the initial colon
" https://github.com/neoclide/coc-sources/issues/15#issuecomment-636398317
if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Show available actions
nnoremap <silent> <leader>ca :CocAction<cr>

if has('nvim-0.4.0') || has('patch-8.2.0750')
    nnoremap <nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
    nnoremap <nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
    inoremap <nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
    inoremap <nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
endif

" highlighting overrides for plugins
hi CocUnderline gui=none cterm=none

let g:coc_global_extensions = [ 'coc-pyright', 'coc-json', 'coc-go', 'coc-rust-analyzer', 'coc-emoji', 'coc-git']
