{ config, pkgs, ... }:
{
  config = {
    programs.chromium = {
      enable = true;
      package = pkgs.google-chrome;
      extensions = [
        "aeblfdkhhhdcdjpifhhbdiojplfjncoa" # 1password
        "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock-origin
        "dbepggeogbaibhgnhhndojpepiihcmeb" # vimium
        "niloccemoadcdkdjlinkgdfekeahmflj" # pocket
      ];
    };
  };
}

