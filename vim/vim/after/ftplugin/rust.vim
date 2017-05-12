" Set the compiler options
let cargo_toml_path = findfile('Cargo.toml', '.;')
if empty(cargo_toml_path)
    " We are compiling a standalone rust file
    compiler rustc
else
    compiler cargo
endif

hi rustCommentLineDoc guifg=#EABB9D

nnoremap <leader>t :update\|:make! check --color always<cr>
nnoremap <leader>r :update\|:make! run --color always<cr>
