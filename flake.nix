{
  description = "home-manager configuration";

  inputs = {
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixgl.url = "github:guibou/nixGL";
    nixgl.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, darwin, flake-utils, home-manager, ... }:
    let
      mkNixOSConfiguration =
        name: nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./system/nixos/${name}/configuration.nix
          ];
        };

      appendNixOSConfiguration =
        attrs: name: attrs // {
          "${name}" = mkNixOSConfiguration name;
        };

      nixOsConfigurations =
        names: {
          nixosConfigurations = builtins.foldl' appendNixOSConfiguration { } names;
        };

      # these definitions are per system
      perSystemConfigurations = flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
          };

          # overlays
          overlays = [
            (final: prev: {
              listprojects = final.callPackage ./derivations/listprojects/default.nix { };
              brave =
                if pkgs.stdenv.isDarwin then
                  (final.callPackage ./derivations/brave/default.nix { })
                else prev.brave;
            })
          ];
        in
        {
          homeConfigurations = {
            simon = home-manager.lib.homeManagerConfiguration {
              pkgs = pkgs;
              modules = [
                { nixpkgs.overlays = overlays; }
                ./home/home.nix
              ];
            };
          };
          darwinConfigurations = {
            mba = darwin.lib.darwinSystem {
              system = "aarch64-darwin";
              modules = [
                ./system/darwin/configuration.nix
              ];
              inputs = { inherit darwin pkgs; };
            };
          };
          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              python310
              python310Packages.black
            ];
          };
        }
      );
    in
    nixOsConfigurations [
      "nixos"
    ] // perSystemConfigurations;
}
