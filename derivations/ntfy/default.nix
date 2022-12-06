{ pkgs ? import <nixpkgs> { } }:
let

  sources = {
    x86_64-linux = (attrs: pkgs.fetchurl {
      url = "https://github.com/binwiederhier/${attrs.pname}/releases/download/v${attrs.version}/${attrs.pname}_${attrs.version}_linux_x86_64.tar.gz";
      hash = "sha256-gffkZ8Qa75o9eNbQEz/I4QVZcVQW6xIaFBdujuK9CuU=";
    });

    x86_64-darwin = (attrs: pkgs.fetchurl {
      url = "https://github.com/binwiederhier/${attrs.pname}/releases/download/v${attrs.version}/${attrs.pname}_${attrs.version}_macOS_all.tar.gz";
      hash = "";
    });

    aarch64-darwin = (attrs: pkgs.fetchurl {
      url = "https://github.com/binwiederhier/${attrs.pname}/releases/download/v${attrs.version}/${attrs.pname}_${attrs.version}_macOS_all.tar.gz";
      hash = "sha256-fPg/WHIQpWr2mN6WBh5wtYfQRY7CuSC65niNgheEYFc=";
    });
  };
in
pkgs.stdenv.mkDerivation (attrs: {
  pname = "ntfy";
  version = "1.29.1";

  src = sources.${pkgs.stdenv.system} { inherit (attrs) pname version; };

  installPhase = ''
    mkdir -p $out/bin
    cp ${ attrs. pname} $out/bin
  '';
})
