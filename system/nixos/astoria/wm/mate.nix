{ pkgs, ... }:
{
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.mate = {
    enable = true;
  };
  environment.systemPackages = [
    pkgs.gnome.gnome-themes-extra
    pkgs.mate.mate-tweak
  ];
  services.blueman.enable = true;
  environment.mate.excludePackages = with pkgs; [
    mate.mate-terminal
  ];
  services.picom.enable = true;
}
