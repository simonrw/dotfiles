setlocal textwidth=100
setlocal nowrap
setlocal nocindent
setlocal formatoptions-=t
setlocal formatoptions-=q
setlocal formatoptions-=c

nnoremap md :s/TODO/DONE/g<cr>
nnoremap mt :s/DONE/TODO/g<cr>
