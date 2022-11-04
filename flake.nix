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
          overlays = [
            (final: prev: {
              listprojects = final.callPackage ./derivations/listprojects/default.nix { };
              brave =
                if final.stdenv.isDarwin then
                  (final.callPackage ./derivations/brave/default.nix { })
                else prev.brave;
            })
            # override the version of xattr for poetry
            (
              let
                xattr-override = {
                  packageOverrides = pyself: pysuper: {
                    xattr = pysuper.xattr.overrideAttrs (o: rec {
                      pname = o.pname;
                      version = "0.9.9";
                      src = pysuper.fetchPypi {
                        inherit pname version;
                        sha256 = "09cb7e1efb3aa1b4991d6be4eb25b73dc518b4fe894f0915f5b0dcede972f346";
                      };
                    });
                  };
                };
              in
              self: super: {
                python310 = super.python310.override xattr-override;
                python39 = super.python39.override xattr-override;
                python38 = super.python38.override xattr-override;
              }
            )
          ];

          pkgs = import nixpkgs {
            inherit system overlays;
            config.allowUnfree = true;
          };

        in
        {
          homeConfigurations = {
            simon = home-manager.lib.homeManagerConfiguration {
              pkgs = pkgs;
              modules = [
                ./home/modules/darwin.nix
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
