{
  description = "home-manager configuration";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/NUR";
    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    jetbrains-updater = {
      url = "gitlab:genericnerdyusername/jetbrains-updater";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    vscode-server = {
      url = "github:msteen/nixos-vscode-server";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    cert-info = {
      url = "github:simonrw/cert-info";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    simpleproxy = {
      url = "github:simonrw/simpleproxy";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    # vim plugins
    plugin-vim-tmux-runner = {
      url = "github:christoomey/vim-tmux-runner";
      flake = false;
    };
    catppuccin-delta = {
      url = "github:catppuccin/delta";
      flake = false;
    };
    testsearch = {
      url = "github:simonrw/testsearch";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs = {
    self,
    nixpkgs,
    darwin,
    flake-utils,
    home-manager,
    jetbrains-updater,
    vscode-server,
    cert-info,
    nur,
    nixvim,
    ...
  } @ inputs: let
    mkOverlays = system: [
      (final: prev: {
        testsearch = inputs.testsearch.packages.${system}.default;
        keymapp = final.callPackage ./derivations/keymapp {pkgs = final;};
        listprojects = final.callPackage ./derivations/listprojects {};
        notify-wrapper = final.callPackage ./derivations/notify-wrapper {};
        monaspace = final.callPackage ./derivations/monaspace {};
        ansi = final.callPackage ./derivations/ansi {};
        wally = final.callPackage ./derivations/wally {};
        cert-info = cert-info.packages.${system}.default;
        simpleproxy = inputs.simpleproxy.packages.${system}.default;
        gh-repo-url = final.callPackage ./derivations/gh-repo-url {};
        gh-pr-url = final.callPackage ./derivations/gh-pr-url {};
        gh-rebase-pr = final.callPackage ./derivations/gh-rebase-pr {};
        wlman = final.callPackage ./derivations/wlman {};
        check-certificate-revocation = final.callPackage ./derivations/check-certificate-revocation {};
        neovim = self.packages.${system}.nixvim;
        # https://kokada.capivaras.dev/blog/quick-bits-realise-nix-symlinks/
        realise-symlink = final.writeShellApplication {
            name = "realise-symlink";
            runtimeInputs = with final; [ coreutils ];
            text = ''
              for file in "$@"; do
                if [[ -L "$file" ]]; then
                  if [[ -d "$file" ]]; then
                    tmpdir="''${file}.tmp"
                    mkdir -p "$tmpdir"
                    cp --verbose --recursive "$file"/* "$tmpdir"
                    unlink "$file"
                    mv "$tmpdir" "$file"
                    chmod --changes --recursive +w "$file"
                  else
                    cp --verbose --remove-destination "$(readlink "$file")" "$file"
                    chmod --changes +w "$file"
                  fi
                else
                  >&2 echo "Not a symlink: $file"
                  exit 1
                fi
              done
            '';
          };
      })
      # override the version of xattr for poetry
      (
        let
          python-overrides = self: {
            packageOverrides = _: pysuper: {
              cherrypy = pysuper.cherrypy.overrideAttrs (_: {
                doInstallCheck = !self.stdenv.isDarwin;
              });
              debugpy = pysuper.debugpy.overrideAttrs (_: {
                doInstallCheck = !self.stdenv.isDarwin;
              });
            };
          };
        in
          self: super: {
            python310 = super.python310.override (python-overrides self);
            python39 = super.python39.override (python-overrides self);
            python38 = super.python38.override (python-overrides self);
          }
      )
      jetbrains-updater.overlay
      nur.overlay
    ];

    getPkgs = system:
      import nixpkgs {
        inherit system;
        overlays = mkOverlays system;
        config.allowUnfree = true;
        config.permittedInsecurePackages = [
          # for obsidian
          "electron-25.9.0"
        ];
      };

    mkNixOSConfiguration = system: name: let
      pkgs = getPkgs system;
    in
      nixpkgs.lib.nixosSystem {
        inherit pkgs system;
        specialArgs = {
          inherit inputs;
        };
        modules = [
          self.modules.nix
          (self.modules.nixos {inherit name;})
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = {
              inherit system inputs;
              isLinux = pkgs.stdenv.isLinux;
              isDarwin = pkgs.stdenv.isDarwin;
            };

            home-manager.users.simon = {...}: {
              imports = [
                ./home/home.nix
                inputs.nix-index-database.hmModules.nix-index
              ];

              home.packages = [
                self.packages.${system}.nixvim
              ];
            };
          }
          vscode-server.nixosModule
          ({
            config,
            pkgs,
            ...
          }: {
            services.vscode-server.enable = true;
          })
        ];
      };

    appendNixOSConfiguration = attrs: {
      system,
      name,
    }:
      attrs
      // {
        "${name}" = mkNixOSConfiguration system name;
      };

    nixOsConfigurations = systemDefinitions: {
      nixosConfigurations = builtins.foldl' appendNixOSConfiguration {} systemDefinitions;
    };

    darwinConfigurations = {
      darwinConfigurations = let
        system = "aarch64-darwin";

        pkgs = getPkgs system;

        mkDarwinConfiguration = { hostname }: darwin.lib.darwinSystem {
           inherit pkgs system;
           specialArgs = {
              inherit hostname;
           };
          modules = [
            self.modules.nix
            (self.modules.darwin {
              name = "common";
            })
            home-manager.darwinModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = {
                inherit system inputs hostname;
                isLinux = pkgs.stdenv.isLinux;
                isDarwin = pkgs.stdenv.isDarwin;
              };

              home-manager.users.simon = {...}: {
                imports = [
                  ./home/home.nix
                  inputs.nix-index-database.hmModules.nix-index
                ];

                home.packages = [
                  self.packages.${system}.nixvim
                ];
              };
            }
          ];
        };
      in {
        mba = mkDarwinConfiguration { hostname = "mba"; };
        mm = mkDarwinConfiguration { hostname = "mm"; };
      };
    };

    # these definitions are per system
    perSystemConfigurations = flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = getPkgs system;
      in {
        inherit pkgs;
        formatter = pkgs.alejandra;

        homeConfigurations = {
          simon = home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [
              ./home/home.nix
              inputs.nix-index-database.hmModules.nix-index
              ({ ... }: {
              home.packages = [
                self.packages.${system}.nixvim
              ];
            })
            ];
            # stop infinite recusion when trying to access
            # pkgs.stdenv.is{Linux,Darwin} from within a module
            extraSpecialArgs = {
              inherit system inputs;
              isLinux = pkgs.stdenv.isLinux;
              isDarwin = pkgs.stdenv.isDarwin;
            };
          };
          minimal = home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [
              ./minimal/home.nix
              ({...}: {
                home.packages = [
                  self.packages.${system}.nixvim
                ];
              })
            ];
            # stop infinite recusion when trying to access
            # pkgs.stdenv.is{Linux,Darwin} from within a module
            extraSpecialArgs = {
              inherit system inputs;
              isLinux = pkgs.stdenv.isLinux;
              isDarwin = pkgs.stdenv.isDarwin;
            };
          };
        };
        devShells.default =
          pkgs.mkShell
          {
            buildInputs = with pkgs; [
              nixd
            ];
          };

          packages.nixvim = inputs.nixvim.legacyPackages.${system}.makeNixvimWithModule {
            inherit pkgs;
            module = {
              imports = [
                ./home/nixvim/default.nix
              ];
            };
            extraSpecialArgs = {
            };
          };
      }
    );

    modules.modules = {
      # common module to configure nix
      nix = {...}: {
        nix.registry.nixpkgs.flake = nixpkgs;
        # set the system "nixpkgs" to the nixpkgs defined in this flake
        # https://dataswamp.org/~solene/2022-07-20-nixos-flakes-command-sync-with-system.html#_nix-shell_vs_nix_shell
        nix.nixPath = ["nixpkgs=/etc/channels/nixpkgs"];
        environment.etc."channels/nixpkgs".source = nixpkgs.outPath;
      };
      nixos = {name ? throw "No module name provided" }: import ./system/nixos/${name}/configuration.nix;
      darwin = {name ? throw "No module name provided" }: import ./system/darwin/${name}/configuration.nix;
    };
  in
    nixOsConfigurations
    [
      {
        name = "astoria";
        system = "x86_64-linux";
      }
    ]
    // darwinConfigurations
    // perSystemConfigurations
    // modules;
}
