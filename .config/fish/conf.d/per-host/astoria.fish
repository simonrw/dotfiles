# Use the external drive for cargo builds and go package downloads
set -l __LOCATION /mnt/data

if test -d $__LOCATION
    set -gx CARGO_TARGET_DIR $__LOCATION/cargo-target
    set -gx CARGO_HOME $__LOCATION/cargo-home
    set -gx RUSTUP_HOME $__LOCATION/rustup-home
    set -gx GOPATH $__LOCATION/gocode
    fish_add_path $CARGO_HOME/bin $GOPATH/bin
end
