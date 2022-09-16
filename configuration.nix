{ pkgs, lib, ... }:
{
  environment.systemPackages = [
  ];

  users.users.simon = {
    name = "simon";
    home = "/Users/simon";
    shell = pkgs.fish;
  };

  environment.shells = [
    pkgs.fish
  ];

  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;
  documentation.enable = true;
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
    auto-optimise-store = true
    experimental-features = nix-command flakes
  '' + lib.optionalString (pkgs.system == "aarch64-darwin") ''
    extra-platforms = x86_64-darwin aarch64-darwin
  '';

  programs.fish.enable = true;
  programs.zsh.enable = true;
  programs.nix-index.enable = true;

  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToEscape = true;

  # system.activationScripts.extraActivation.text = ''
  #   # For TouchID to work in `op` 1Password CLI, it needs to be at `/usr/local/bin`
  #   # (Hopefully this requirement will be lifted by 1Password at some point)
  #   # NOTE we don't install `op` via nix but simply copy the binary
  #   cp ${pkgs._1password}/bin/op /usr/local/bin/op
  # '';
}
