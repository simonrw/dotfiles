{ pkgs ? import <nixpkgs> { } }:
let
  inherit (pkgs) fetchFromGitHub;
in
pkgs.rustPlatform.buildRustPackage rec {
  pname = "listprojects";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "mindriot101";
    repo = "listprojects";
    rev = "4b94543c1943d8f772ef14283f6276333d5a98b1";
    sha256 = "sha256-I+7bGfk0GSI7TRhPGPqYuT4eA5BspUELrqMO4l829G0=";
  };

  cargoHash = "sha256-2mY9c9dxwd0FeO1R/VA0FG1zNljPckipF32ASzMPJxo=";
}
