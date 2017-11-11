" Set the compiler options
let cargo_toml_path = findfile('Cargo.toml', '.;')
if empty(cargo_toml_path)
    " We are compiling a standalone rust file
    compiler rustc
    " nnoremap <leader>t :update\|:Make<cr>
else
    compiler cargo
    nnoremap <leader>t :update\|:Make check<cr>
    nnoremap <leader>r :update\|:Start cargo run<cr>
endif

hi rustCommentLineDoc guifg=#EABB9D
let g:rustfmt_autosave = 1
let g:racer_experimental_completer = 1
let g:racer_cmd = "/Users/simon/.cargo/bin/racer"
