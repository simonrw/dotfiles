{ pkgs ? import <nixpkgs> { } }:
pkgs.stdenv.mkDerivation (final: {
  pname = "notion";
  version = "0.0.6";

  src = builtins.fetchurl {
    url = "https://github.com/simonrw/notion-app/releases/download/v${final.version}/Notion-${final.version}.AppImage";
    sha256 = "sha256:1x3fg0q0ni66kp7qvs27dpj4zyb9v7q646xyq8mmbw0xgm6w4i3w";
  };

  appimageContents = pkgs.appimageTools.extractType2 {
    name = "notion";
    inherit (final) src;
  };

  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;

  nativeBuildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/share/notion $out/share/applications
    cp -a ${final.appimageContents}/{locales,resources} $out/share/notion
    cp -a ${final.appimageContents}/notion.desktop $out/share/applications/notion.desktop
    cp -a ${final.appimageContents}/usr/share/icons $out/share
    substituteInPlace $out/share/applications/notion.desktop \
      --replace 'Exec=AppRun' "Exec=$out/bin/notion"

    runHook postInstall
  '';

  postFixup = ''
    makeWrapper ${pkgs.electron}/bin/electron $out/bin/notion \
      --add-flags $out/share/notion/resources/app.asar
  '';

})
