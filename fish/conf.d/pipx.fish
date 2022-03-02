set -x PIPX_BIN_DIR {$BUILD_PREFIX}/pipx/bin
set -x PATH {$PIPX_BIN_DIR} {$PATH}
set -x PIPX_DEFAULT_PYTHON {$ASDF_DIR}/shims/python

