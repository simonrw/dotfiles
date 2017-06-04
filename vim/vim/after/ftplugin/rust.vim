" Set the compiler options
let cargo_toml_path = findfile('Cargo.toml', '.;')
if empty(cargo_toml_path)
    " We are compiling a standalone rust file
    compiler rustc
else
    compiler cargo
endif

hi rustCommentLineDoc guifg=#EABB9D

nnoremap <leader>t :update\|:Make check<cr>
nnoremap <leader>r :update\|:Make run<cr>
nnoremap <leader>w :update\|:Make test<cr>
