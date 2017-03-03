nnoremap <silent> K :call LanguageClient_textDocument_hover()<Cr>
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> <leader>x :Neomake!<cr>

" Set the compiler options
let cargo_toml_path = findfile('Cargo.toml', '.;')
if empty(cargo_toml_path)
    " We are compiling a standalone rust file
    compiler rustc
else
    compiler cargo
endif
