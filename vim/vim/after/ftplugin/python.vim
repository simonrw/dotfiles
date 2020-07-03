let @i='import IPython; IPython.embed(); exit()'
let @p='import pudb; pudb.set_trace()'
let @n='if __name__ == "__main__":'
set nowrap
set textwidth=0

" Set up auto formatting
if executable('black')
    nnoremap <leader>y mm:%!black --fast -q -<Cr>`m
    vnoremap <leader>y :'<,'>!black --fast -q -<Cr>
endif

vnoremap <silent> <leader>t :VtrSendLinesToRunner<Cr>
nnoremap <silent> <leader>p vip:VtrSendLinesToRunner<Cr>

let g:ale_fixers = ["black"]

" enable indent guides and automatically disable afterwards
augroup filetype_python
    autocmd!
    autocmd BufEnter * if &ft ==# 'python' | IndentGuidesEnable | endif
    autocmd BufLeave * if &ft ==# 'python' | IndentGuidesDisable | endif
augroup end

setlocal colorcolumn=80
