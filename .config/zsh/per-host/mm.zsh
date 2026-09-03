# use my external HDD for cargo builds and go package downloads
__LOCATION=/Volumes/External

if test -d ${__LOCATION}; then
    export GOPATH=${__LOCATION}/gocode
    export OLLAMA_MODELS=${__LOCATION}/ollama
    export PATH=${CARGO_HOME}/bin:${GOPATH}/bin:${PATH}
    export COLIMA_HOME=${__LOCATION}/colima
fi
