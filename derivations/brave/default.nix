{ pkgs ? import <nixpkgs> { } }:
let
  inherit (pkgs) system fetchFromGitHub stdenv fetchurl;

  pname = "brave";
  version = "1.45.108";
  shas = {
    # universal pkg means the same sha
    aarch64-darwin = "44a700f70cdfe765a66884d65fa1d20b1124bfb0aadbb933d3ef9e5ff5ed1589";
    x86_64-darwin = "44a700f70cdfe765a66884d65fa1d20b1124bfb0aadbb933d3ef9e5ff5ed1589";
  };

  darwin = stdenv.mkDerivation rec {
    inherit pname version;

    buildInputs = with pkgs; [
      cpio
      xar
    ];

    src = fetchurl {
      url = "https://github.com/brave/brave-browser/releases/download/v${version}/Brave-Browser-universal.pkg";
      sha256 = shas.${system};
    };

    unpackPhase = ''
      xar -xf $src
    '';

    buildPhase = ''
      cat Payload | gunzip -dc | cpio -i
    '';

    installPhase = ''
      mkdir -p $out/Applications
      cp -r 'Brave Browser.app' $out/Applications
    '';

  };
  linux = { };
in
if stdenv.isDarwin
then darwin
else linux
