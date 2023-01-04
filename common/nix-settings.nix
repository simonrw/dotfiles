{ pkgs ? import <nixpkgs> { } }:
{
  gc = {
    automatic = true;
    interval = {
      Weekday = 0;
      Hour = 2;
    };
  };
  settings = {
    auto-optimise-store = true;
    trusted-users = [ "root" "simon" ];
    experimental-features = [ "nix-command" "flakes" ];

    keep-outputs = true;
    keep-derivations = true;

    substituters = [
      "https://mindriot101-home.cachix.org"
      "https://snslistener.cachix.org"
      "https://tree-grepper.cachix.org"
    ];
    trusted-public-keys = [
      "mindriot101-home.cachix.org-1:6M7zAD5oYiwfVPobA5LPtwM+5FewBv4fEMvCgrLACR4="
      "snslistener.cachix.org-1:hUizuaVKug4WDmgLEiErlCMsoDW5+dyJWP0b0mfN/IY="
      "tree-grepper.cachix.org-1:Tm/owXM+dl3GnT8gZg+GTI3AW+yX1XFVYXspZa7ejHg="
    ];
  };
  extraOptions = ''
    # https://jackson.dev/post/nix-reasonable-defaults/
    connect-timeout = 5
    log-lines = 25

    fallback = true
    warn-dirty = false
  '' + pkgs.lib.optionalString (pkgs.system == "aarch64-darwin") ''
    extra-platforms = x86_64-darwin aarch64-darwin
  '';
}
