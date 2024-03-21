{pkgs ? import <nixpkgs> {}}: let
  # settings are different between nixos and nix-darwin
  interval =
    if pkgs.stdenv.isLinux
    then {
      dates = "weekly";
    }
    else {
      interval = {
        Day = 0;
      };
    };
in {
  package = pkgs.nixUnstable;
  gc =
    {
      automatic = false;
      options = "--delete-older-than 7d";
    }
    // interval;
  settings = {
    auto-optimise-store = true;
    trusted-users = ["root" "simon"];
    experimental-features = ["nix-command" "flakes" "ca-derivations"];

    keep-outputs = false;
    keep-derivations = true;

    substituters = [
      "https://mindriot101-home.cachix.org"
      "https://nix-community.cachix.org"
      "https://cftail.cachix.org"
      "https://snslistener.cachix.org"
    ];
    trusted-public-keys = [
      "mindriot101-home.cachix.org-1:6M7zAD5oYiwfVPobA5LPtwM+5FewBv4fEMvCgrLACR4="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "cftail.cachix.org-1:/TvaKTN25wL/+JMBEkOmxM03HWUDY3UcuMa5ld3PPXw="
      "snslistener.cachix.org-1:hUizuaVKug4WDmgLEiErlCMsoDW5+dyJWP0b0mfN/IY="
    ];
  };
  extraOptions =
    ''
      # https://jackson.dev/post/nix-reasonable-defaults/
      connect-timeout = 5
      log-lines = 25

      fallback = true
      warn-dirty = false

      # clean up disk space when nearly full
      # - free up to 2 GiB when there is less than 1 GiB
      # https://nixos.wiki/wiki/Storage_optimization
      min-free = ${toString (1024 * 1024 * 1024)}        # 1 GiB
      max-free = ${toString (2 * 1024 * 1024 * 1024)}    # 2 GiB
    ''
    + pkgs.lib.optionalString (pkgs.system == "aarch64-darwin") ''
      extra-platforms = x86_64-darwin
    '';
}
