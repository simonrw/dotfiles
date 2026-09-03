# use the external drive for cargo builds and go package downloads
__LOCATION=/mnt/data

if test -d ${__LOCATION}; then
    export GOPATH=${__LOCATION}/gocode
    export OLLAMA_MODELS=${__LOCATION}/ollama
    export PATH=${CARGO_HOME}/bin:${GOPATH}/bin:${PATH}
fi

