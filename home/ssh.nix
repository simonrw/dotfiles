{ ... }:
{
  programs.ssh = {
    enable = true;
    controlMaster = "auto";
    controlPersist = "5m";
    matchBlocks = {
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
