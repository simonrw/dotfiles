{pkgs ? import <nixpkgs> {}}:
with pkgs;
  stdenv.mkDerivation (finalAttrs: {
    pname = "keymapp";
    version = "1.0.4";

    src = fetchzip {
      url = "https://oryx.nyc3.cdn.digitaloceanspaces.com/keymapp/keymapp-${finalAttrs.version}.tar.gz";
      hash = "sha256-0ej6nbeZ33HYoAotI07qoJVGw87YKRi5KSzO44nZdDE=";
    };

    nativeBuildInputs = [
      autoPatchelfHook
      copyDesktopItems
    ];

    desktopItems = [
      (makeDesktopItem rec {
        name = "Keymapp";
        exec = finalAttrs.pname;
        icon = finalAttrs.pname;
        desktopName = name;
        genericName = name;
        categories = [
          "System"
        ];
      })
    ];

    installPhase = ''
      mkdir -p $out/share/applications
      copyDesktopItems
      install -Dm755 keymapp $out/bin/keymapp
    '';

    buildInputs = [
      libusb1
      eudev
      webkitgtk
      gtk3
      gdk-pixbuf
      glib
      webkitgtk
    ];
  })
