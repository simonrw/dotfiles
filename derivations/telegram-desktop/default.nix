{ pkgs ? import <nixpkgs> { } }:
pkgs.stdenv.mkDerivation (final: {
  pname = "telegram-desktop";
  version = "4.5.0";

  src = pkgs.fetchurl {
    url = "https://updates.tdesktop.com/tlinux/tsetup.${final.version}.tar.xz";
    sha256 = "sha256-aKdV1Oj2AYBh8uPAZEnezXKR30NjlaWfYxJKD0k3GZE=";
  };

  nativeBuildInputs = [
    pkgs.autoPatchelfHook
  ];

  buildInputs = [
    pkgs.fontconfig
    pkgs.xorg.libX11
    pkgs.glib
    pkgs.libglvnd
  ];

  installPhase = ''
    install -m755 -D Telegram $out/bin/telegram
  '';

  meta = with pkgs.lib; {
    homepage = "https://desktop.telegram.org/";
    description = "Fast and secure desktop app, perfectly synced with your mobile phone.";
    platforms = platforms.linux;
  };
})
