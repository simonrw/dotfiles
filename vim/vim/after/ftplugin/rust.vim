" Set the compiler options
let cargo_toml_path = findfile('Cargo.toml', '.;')
if empty(cargo_toml_path)
    compiler rustc
else
    compiler cargo
endif

let g:rustfmt_autosave = 1
let g:rustfmt_fail_silently = 1
let g:racer_experimental_completer = 1
let g:racer_cmd = "/Users/simon/.cargo/bin/racer"

if executable('rustfmt')
    nnoremap <leader>y :RustFmt<Cr>
endif

" Use omnifunc for autocompletion
setlocal omnifunc=lsp#complete
setlocal colorcolumn=+1
setlocal textwidth=100
