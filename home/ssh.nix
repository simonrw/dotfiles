{...}: {
  programs.ssh = {
    enable = true;
    compression = false;
    includes = [
      "config_local"
    ];
    controlMaster = "no";
    controlPersist = "10m";
    extraConfig = ''
      EnableEscapeCommandline=yes
    '';
    matchBlocks = {
      "*" = {
        sendEnv = ["TMUX_DISABLED"];
      };
      "astoria" = {
        user = "simon";
        hostname = "astoria";
        setEnv = {
          LC_ALL = "C";
        };
      };
      "pi3" = {
        user = "pi";
        hostname = "pi3";
        identityFile = "~/.ssh/id_ed25519";
      };
    };
  };
}
