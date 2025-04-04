{pkgs ? import <nixpkgs> {}}: let
  python-interpreter = pkgs.python3.withPackages (p: [
    p.requests
  ]);
in
  pkgs.stdenv.mkDerivation {
    name = "notify-wrapper";
    version = "0.1.0";

    buildInputs = [
      python-interpreter
    ];

    src = pkgs.lib.cleanSource ./.;

    buildPhase = ":";
    installPhase = ''
      install -D -m 0755 ./notify_wrapper.py $out/bin/notify-wrapper
    '';
  }
