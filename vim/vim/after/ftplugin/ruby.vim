set tabstop=2
set shiftwidth=2

" Aliases for rails projects
nmap <buffer> <leader>gM :CtrlP app/models<cr>
nmap <buffer> <leader>gV :CtrlP app/views<cr>
nmap <buffer> <leader>gC :CtrlP app/controllers<cr>
nmap <buffer> <leader>gA :CtrlP app/assets<cr>
nmap <buffer> <leader>gL :CtrlP lib<cr>
nmap <buffer> <leader>gM :CtrlP db/migrate<cr>
nmap <buffer> <leader>gS :CtrlP spec<cr>

let b:vimpipe_command="ruby"
