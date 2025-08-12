# use my external HDD for cargo builds and go package downloads
if test -d /Volumes/External
    set -x CARGO_TARGET_DIR /Volumes/External/cargo-target
    set -x CARGO_HOME /Volumes/External/cargo-home
    set -x RUSTUP_HOME /Volumes/External/rustup-home

    set -x GOPATH /Volumes/External/gocode
    set -x PATH {$CARGO_HOME}/bin {$GOPATH}/bin {$PATH}
end
