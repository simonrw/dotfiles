" Set the compiler options
let cargo_toml_path = findfile('Cargo.toml', '.;')
if empty(cargo_toml_path)
    compiler rustc
else
    compiler cargo
endif

" configure ale
let b:ale_linters = ['analyzer', 'cargo']
let b:ale_fixers = ["rustfmt"]
