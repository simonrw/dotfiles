# Use external HDD for cargo builds and go package downloads
set -l __LOCATION /Volumes/External

if test -d $__LOCATION; and test -r $__LOCATION; and test -w $__LOCATION
    set -gx CARGO_TARGET_DIR $__LOCATION/cargo-target
    set -gx CARGO_HOME $__LOCATION/cargo-home
    set -gx RUSTUP_HOME $__LOCATION/rustup-home
    set -gx GOPATH $__LOCATION/gocode
    fish_add_path $CARGO_HOME/bin $GOPATH/bin
    set -gx OLLAMA_MODELS $__LOCATION/ollama
    set -gx COLIMA_HOME $__LOCATION/colima
end
