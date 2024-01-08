{pkgs ? import <nixpkgs> {}}:
pkgs.stdenv.mkDerivation {
  name = "project";
  version = "0.1.0";

  buildInputs = [
    pkgs.python3
    pkgs.fzf
    pkgs.fd
    pkgs.tmux
  ];

  src = pkgs.lib.cleanSource ./.;

  postPatch = ''
    substituteInPlace project.py \
      --replace \
      '"fzf-tmux"' "\"${pkgs.fzf}/bin/fzf-tmux\""

    substituteInPlace project.py \
      --replace \
      '"fzf"' "\"${pkgs.fzf}/bin/fzf\""

    substituteInPlace project.py \
      --replace \
      '"fd"' "\"${pkgs.fd}/bin/fd\""

    substituteInPlace project.py \
      --replace \
      '"tmux"' "\"${pkgs.tmux}/bin/tmux\""
  '';

  buildPhase = ":";
  installPhase = ''
    install -D -m 0755 ./project.py $out/bin/project
  '';
}
