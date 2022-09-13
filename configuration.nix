{ pkgs, ... }:
{
  environment.systemPackages = [
  ];

  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;
  documentation.enable = true;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  fonts.fonts = with pkgs; [
    source-code-pro
  ];
}
