{...}: {
  programs.zsh = {
    enable = false;
    autosuggestion.enable = true;
    enableCompletion = true;
    enableVteIntegration = true;
    dotDir = ".config/zsh";
    history = {
      expireDuplicatesFirst = true;
      extended = true;
      ignoreAllDups = true;
      ignoreDups = true;
      ignoreSpace = true;
      save = 50000;
      share = true;
    };
  };
  # environment.pathsToLink = [
  #   "/share/zsh"
  # ];
}
