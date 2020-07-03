set sw=2
set ts=2
set expandtab

" enable indent guides and automatically disable afterwards
augroup filetype_yaml
    autocmd!
    autocmd BufEnter * if &ft ==# 'yaml' | IndentGuidesEnable | endif
    autocmd BufLeave * if &ft ==# 'yaml' | IndentGuidesDisable | endif
augroup end

