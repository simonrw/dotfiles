{
    services.ollama = {
        enable = true;
        # Optional: load models on startup
        loadModels = [];
        acceleration = "cuda";
    };
}
