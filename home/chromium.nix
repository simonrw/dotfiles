{ config, pkgs, ... }:
{
  config = {
    programs.brave = {
      enable = false;
      extensions = [
        "aeblfdkhhhdcdjpifhhbdiojplfjncoa" # 1password
        "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock-origin
        "dbepggeogbaibhgnhhndojpepiihcmeb" # vimium
        "niloccemoadcdkdjlinkgdfekeahmflj" # pocket
      ];
    };
  };
}

