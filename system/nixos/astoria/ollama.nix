{
    services.ollama = {
        enable = true;
        # Optional: load models on startup
        loadModels = [];
        acceleration = "cuda";
        host = "0.0.0.0";
    };
}
