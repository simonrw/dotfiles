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
    jetbrains-updater.url = "gitlab:genericnerdyusername/jetbrains-updater";
    jetbrains-updater.inputs.nixpkgs.follows = "nixpkgs";
    nix-index-database.url = "github:Mic92/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
    vscode-server.url = "github:msteen/nixos-vscode-server";
    vscode-server.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { nixpkgs
    , darwin
    , flake-utils
    , home-manager
    , jetbrains-updater
    , nix-index-database
    , vscode-server
    , ...
    }:
    let
      mkOverlays = system: [
        (final: prev: {
          listprojects = final.callPackage ./derivations/listprojects { };
          notify-wrapper = final.callPackage ./derivations/notify-wrapper { };
          notion = final.callPackage ./derivations/notion { };
          telegram-desktop = final.callPackage ./derivations/telegram-desktop { };
          cargo-dist = final.callPackage ./derivations/cargo-dist { };
          database = nix-index-database.legacyPackages.x86_64-linux.database;
        })
        # override the version of xattr for poetry
        (
          let
            python-overrides = self: {
              packageOverrides = _: pysuper: {
                cherrypy = pysuper.cherrypy.overrideAttrs (_: rec {
                  doInstallCheck = !self.stdenv.isDarwin;
                });
                debugpy = pysuper.debugpy.overrideAttrs (_: rec {
                  doInstallCheck = !self.stdenv.isDarwin;
                });
              };
            };
          in
          self: super: {
            python310 = super.python310.override (python-overrides self);
            python39 = super.python39.override (python-overrides self);
            python38 = super.python38.override (python-overrides self);

            # enable a specific python version to run debugpy
            python-for-debugging = super.python3.withPackages (ps: [
              ps.debugpy
            ]);
          }
        )
        jetbrains-updater.overlay
      ];
      mkNixOSConfiguration =
        let
          system = "x86_64-linux";

          pkgs = import nixpkgs {
            inherit system;
            overlays = mkOverlays system;
            config.allowUnfree = true;
          };
        in
        name: nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules =
            [
              {
                nix.registry.nixpkgs.flake = nixpkgs;
              }
              ./system/nixos/${name}/configuration.nix
              home-manager.nixosModules.home-manager
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.extraSpecialArgs = {
                  isLinux = pkgs.stdenv.isLinux;
                  isDarwin = pkgs.stdenv.isDarwin;
                };

                home-manager.users = {
                  simon = import ./home/home.nix;
                };
              }
              nix-index-database.nixosModules.nix-index
              vscode-server.nixosModule
              ({ config, pkgs, ... }: {
                services.vscode-server.enable = true;
              })
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

      darwinConfigurations =
        {
          darwinConfigurations =
            let
              system = "aarch64-darwin";

              pkgs = import nixpkgs {
                inherit system;
                overlays = mkOverlays system;
                config.allowUnfree = true;
              };
            in
            {
              mba = darwin.lib.darwinSystem {
                inherit pkgs system;
                modules = [
                  {
                    nix.registry.nixpkgs.flake = nixpkgs;
                  }
                  ./system/darwin/configuration.nix
                  home-manager.darwinModules.home-manager
                  {
                    home-manager.useGlobalPkgs = true;
                    home-manager.useUserPackages = true;
                    home-manager.extraSpecialArgs = {
                      isLinux = pkgs.stdenv.isLinux;
                      isDarwin = pkgs.stdenv.isDarwin;
                    };

                    home-manager.users.simon = import ./home/home.nix;
                  }
                  nix-index-database.darwinModules.nix-index
                ];
              };
            };
        };

      # these definitions are per system
      perSystemConfigurations = flake-utils.lib.eachDefaultSystem (system:
        let
          overlays = mkOverlays system;

          pkgs = import nixpkgs {
            inherit system overlays;
            config.allowUnfree = true;
          };

        in
        {
          homeConfigurations = {
            simon = home-manager.lib.homeManagerConfiguration {
              inherit pkgs;
              modules = [
                ./home/home.nix
              ];
              # stop infinite recusion when trying to access
              # pkgs.stdenv.is{Linux,Darwin} from within a module
              extraSpecialArgs = {
                isLinux = pkgs.stdenv.isLinux;
                isDarwin = pkgs.stdenv.isDarwin;
              };
            };
          };
          devShells.default = pkgs.mkShell
            {
              buildInputs = with pkgs; [
                python310
                python310Packages.black
              ];
            };
        }
      );
    in
    nixOsConfigurations
      [
        "astoria"
      ] // darwinConfigurations // perSystemConfigurations;
}

