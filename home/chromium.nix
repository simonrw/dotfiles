{isLinux, ...}: {
  config = {
    programs.brave = {
      enable = isLinux;
      extensions = [
        "aeblfdkhhhdcdjpifhhbdiojplfjncoa" # 1password
        "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock-origin
        "dbepggeogbaibhgnhhndojpepiihcmeb" # vimium
        "niloccemoadcdkdjlinkgdfekeahmflj" # pocket
        "lckanjgmijmafbedllaakclkaicjfmnk" # ClearURLs
        "edibdbjcniadpccecjdfdjjppcpchdlm" # I still don't care about cookies
      ];
    };
  };
}
