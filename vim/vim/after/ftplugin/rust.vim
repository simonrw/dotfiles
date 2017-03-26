" Set the compiler options
let cargo_toml_path = findfile('Cargo.toml', '.;')
if empty(cargo_toml_path)
    " We are compiling a standalone rust file
    compiler rustc
else
    compiler cargo
endif
