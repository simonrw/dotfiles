{pkgs, ...}: {
  # Configure steam
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    # https://github.com/NixOS/nixpkgs/issues/236561#issuecomment-1581879353
    package = with pkgs; steam.override {extraPkgs = pkgs: [attr];};
  };
}
