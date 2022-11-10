{ pkgs ? import <nixpkgs> { } }:
let
  traceSeq = pkgs.lib.traceSeq;

  sources = {
    x86_64-linux = (attrs: pkgs.fetchurl {
      url = "https://github.com/binwiederhier/${attrs.pname}/releases/download/v${attrs.version}/${attrs.pname}_${attrs.version}_linux_x86_64.tar.gz";
      hash = "sha256-cJ5Bhf5H0dCZotu5Otjg0udShFeyYoCt3C8xqbuduVk=";
    });

    x86_64-darwin = (attrs: pkgs.fetchurl {
      url = "https://github.com/binwiederhier/${attrs.pname}/releases/download/v${attrs.version}/${attrs.pname}_${attrs.version}_macOS_all.tar.gz";
      hash = pkgs.lib.fakeHash;
    });

    aarch64-darwin = (attrs: pkgs.fetchurl {
      url = "https://github.com/binwiederhier/${attrs.pname}/releases/download/v${attrs.version}/${attrs.pname}_${attrs.version}_macOS_all.tar.gz";
      hash = pkgs.lib.fakeHash;
    });
  };
in
pkgs.stdenv.mkDerivation (attrs: {
  pname = "ntfy";
  version = "1.26.0";

  src = sources.${pkgs.stdenv.system} { inherit (attrs) pname version; };

  installPhase = ''
    mkdir -p $out/bin
    cp ${ attrs. pname} $out/bin
  '';
})
