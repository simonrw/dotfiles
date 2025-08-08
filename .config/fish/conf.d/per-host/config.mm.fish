# use my external HDD for cargo builds and go package downloads
if test -d /Volumes/External
    set -x CARGO_TARGET_DIR /Volumes/External/cargo-target

    set -x GOPATH /Volumes/External/gocode
    set -x PATH {$PATH} {$GOPATH}/bin
end
