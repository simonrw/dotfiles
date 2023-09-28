{ config, pkgs, ... }:
{
  config = {
    programs.brave = {
      enable = true;
      extensions = [
        "aeblfdkhhhdcdjpifhhbdiojplfjncoa" # 1password
        "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock-origin
        "dbepggeogbaibhgnhhndojpepiihcmeb" # vimium
        "niloccemoadcdkdjlinkgdfekeahmflj" # pocket
      ];
    };
  };
}

