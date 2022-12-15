{ pkgs ? import <nixpkgs> { } }:
pkgs.stdenv.mkDerivation (final: {
  pname = "notion";
  version = "0.0.4";

  src = builtins.fetchurl {
    url = "https://github.com/simonrw/notion-app/releases/download/v${final.version}/Notion-${final.version}.AppImage";
    sha256 = "sha256:02bq055haa3lxp2hzmbwqpgwpv69m59w6qahylssxqkg75bm6pjl";
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
