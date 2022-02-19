" Set the compiler options
let cargo_toml_path = findfile('Cargo.toml', '.;')
if empty(cargo_toml_path)
    compiler rustc
else
    compiler cargo
endif

nnoremap <leader>y :silent RustFmt<cr>

" configure ale
let b:ale_linters = ['analyzer', 'cargo']
let b:ale_fixers = ["rustfmt"]
let g:ale_rust_rustfmt_options = "--edition 2021"
let g:ale_rust_cargo_use_clippy = 1
let g:ale_rust_cargo_use_check = 0
