set -x BUILD_PREFIX {$HOME}/.local
set -x PATH {$BUILD_PREFIX}/bin {$HOME}/.bin {$HOME}/.poetry/bin /usr/local/bin {$HOME}/.cargo/bin {$PATH} {$HOME}/bin {$GOPATH}/bin
set -x GOPATH {$HOME}/dev/gocode
set -x EDITOR nvim
set -x REVIEW_BASE main
set -x PYTEST_ADDOPTS "-p no:sugar"
set -x LANG en_GB.UTF-8
set -x CARGO_TARGET_DIR {$HOME}/.cargo/cache
set -x GOPROXY direct
