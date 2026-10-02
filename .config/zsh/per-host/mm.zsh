if [[ -d /Volumes/External && -r /Volumes/External && -w /Volumes/External ]]; then
    export OLLAMA_MODELS=/Volumes/External/ollama
    export COLIMA_HOME=/Volumes/External/colima
fi
