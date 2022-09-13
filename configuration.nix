{ pkgs, lib, ... }:
{
  environment.systemPackages = [
  ];

  users.users.simon = {
  	name = "simon";
	home = "/Users/simon";
	shell = pkgs.fish;
  };

  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;
  documentation.enable = true;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '' + lib.optionalString (pkgs.system == "aarch64-darwin") ''
    extra-platforms = x86_64-darwin aarch64-darwin
  '';

  programs.fish.enable = true;
  programs.zsh.enable = true;
  programs.nix-index.enable = true;

  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToEscape = true;
}
