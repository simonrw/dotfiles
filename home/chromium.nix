{isLinux, ...}: {
  config = {
    programs.google-chrome = {
      enable = isLinux;
      # configuring extensions does not work for google chrome
      # https://github.com/nix-community/home-manager/pull/1867
      # extensions = [
      #   "aeblfdkhhhdcdjpifhhbdiojplfjncoa" # 1password
      #   "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock-origin
      #   "dbepggeogbaibhgnhhndojpepiihcmeb" # vimium
      #   "niloccemoadcdkdjlinkgdfekeahmflj" # pocket
      #   "lckanjgmijmafbedllaakclkaicjfmnk" # ClearURLs
      #   "edibdbjcniadpccecjdfdjjppcpchdlm" # I still don't care about cookies
      # ];
    };
  };
}
