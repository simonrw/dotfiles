set -x BUILD_PREFIX {$HOME}/.local
set -x PATH {$BUILD_PREFIX}/bin {$HOME}/.bin {$HOME}/.poetry/bin /usr/local/bin {$PATH} {$HOME}/bin {$GOPATH}/bin
set -x GOPATH {$HOME}/dev/gocode
set -x EDITOR nvim
set -x REVIEW_BASE main