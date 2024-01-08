# https://nixos.wiki/wiki/Yubikey
{pkgs, ...}: {
  services.udev.packages = [pkgs.yubikey-personalization];
  services.pcscd.enable = true;
  environment.systemPackages = [
    # pkgs.yubioath-flutter
  ];

  # Use yubikey for login
  security.pam.services = {
    login.u2fAuth = true;
    sudo.u2fAuth = true;
  };

  # show prompt
  security.pam.u2f.cue = true;
}
