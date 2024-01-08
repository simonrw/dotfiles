{
  description = "Flake utils demo";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        overlays = [
        ];

        pkgs = import nixpkgs {
          inherit overlays system;
        };
      in {
        devShells = rec {
          default = empty;

          empty = pkgs.mkShell {
            buildInputs = with pkgs; [
            ];
          };

          rust-dev = pkgs.mkShell {
            buildInputs = with pkgs;
              [
                rustc
                cargo
                rust-analyzer
                clippy
                rustfmt
              ]
              ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
                libiconv
              ];

            shellHook = ":";

            RUST_SRC_PATH = "${pkgs.rustPlatform.rustLibSrc}";
          };
        };
      }
    );
}
