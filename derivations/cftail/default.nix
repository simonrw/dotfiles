{ pkgs ? import <nixpkgs> { } }:
let
  inherit (pkgs) fetchFromGitHub;
in
pkgs.rustPlatform.buildRustPackage rec {
  pname = "cftail";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "simonrw";
    repo = "cftail";
    rev = "ebdb2eb7736e11f49d4d670954bc68c4bd0d96b7";
    hash = "sha256-XzPvjfV5rg7lWhU5RIdCwyQWKkS1oHpOlMamG+Zv+ZU=";
  };

  cargoHash = "sha256-RVeI3QBHW8N4ZpoWGVOTPf96EYk3oBGGsCdntbbbWAs=";
}
