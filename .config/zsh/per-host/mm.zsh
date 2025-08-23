# use my external HDD for cargo builds and go package downloads
if test -d /Volumes/External; then
    export CARGO_TARGET_DIR=/Volumes/External/cargo-target
    export CARGO_HOME=/Volumes/External/cargo-home
    export RUSTUP_HOME=/Volumes/External/rustup-home
    export GOPATH=/Volumes/External/gocode
    export PATH=${CARGO_HOME}/bin:${GOPATH}/bin:${PATH}
fi
