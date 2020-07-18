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
let g:racer_cmd = $HOME . "/.cargo/bin/racer"

if executable('rustfmt')
    nnoremap <silent> <leader>y :update\|:RustFmt<Cr>
endif

" configure ale
let b:ale_linters = {'rust': ['analyzer']}
