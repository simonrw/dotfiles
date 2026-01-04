# use the external drive for cargo builds and go package downloads
__LOCATION=/mnt/data

if test -d ${__LOCATION}; then
    export CARGO_TARGET_DIR=${__LOCATION}/cargo-target
    export CARGO_HOME=${__LOCATION}/cargo-home
    export RUSTUP_HOME=${__LOCATION}/rustup-home
    export GOPATH=${__LOCATION}/gocode
    export PATH=${CARGO_HOME}/bin:${GOPATH}/bin:${PATH}
fi

