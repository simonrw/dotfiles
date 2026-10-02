if [[ -d /mnt/data && -r /mnt/data && -w /mnt/data ]]; then
    export OLLAMA_MODELS=/mnt/data/ollama
fi
