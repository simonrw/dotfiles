{pkgs ? import <nixpkgs> {}}: let
  python-interpreter = pkgs.python3.withPackages (p: [
    p.rich
    p.requests
  ]);
in
  pkgs.stdenv.mkDerivation {
    name = "localdocs";
    version = "0.1.0";

    buildInputs = [
      python-interpreter
    ];

    src = pkgs.lib.cleanSource ./.;

    buildPhase = ":";
    installPhase = ''
      install -D -m 0755 ./localdocs.py $out/bin/localdocs
    '';
  }
