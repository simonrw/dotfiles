{pkgs ? import <nixpkgs> {}}:
with pkgs;
  stdenv.mkDerivation (attrs: {
    pname = "ansi";
    version = "3.0.1";

    src = fetchFromGitHub {
      owner = "fidian";
      repo = "ansi";
      rev = attrs.version;
      hash = "sha256-udZ24zsRci0XNfXFb6Nmckzi22EaBVV/51AFkfad5eE=";
    };

    doBuild = false;
    installPhase = ''
      install -Dm755 ansi $out/bin/ansi
    '';
  })
