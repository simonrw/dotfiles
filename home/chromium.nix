{ config, pkgs, ... }:
{
  config = {
    programs.chromium = {
      enable = true;
      package = pkgs.brave;
      extensions = [
        "aeblfdkhhhdcdjpifhhbdiojplfjncoa" # 1password
        "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock-origin
        "dbepggeogbaibhgnhhndojpepiihcmeb" # vimium
        "niloccemoadcdkdjlinkgdfekeahmflj" # pocket
      ];
    };
  };
}

