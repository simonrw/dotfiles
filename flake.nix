{
  description = "home-manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
    };
    darwin = {
      url = "github:lnl7/nix-darwin/nix-darwin-24.11";
    };
    jetbrains-updater = {
      url = "gitlab:genericnerdyusername/jetbrains-updater";
    };
    nix-index-database = {
      url = "github:Mic92/nix-index-database";
    };
    vscode-server = {
      url = "github:msteen/nixos-vscode-server";
    };
    cert-info = {
      url = "github:simonrw/cert-info";
    };
    simpleproxy = {
      url = "github:simonrw/simpleproxy";
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
    };
    listprojects = {
      url = "github:simonrw/listprojects";
    };
    logtimes = {
      url = "github:simonrw/logtimes";
    };
    notestools = {
      url = "github:simonrw/notestools";
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
    ...
  } @ inputs: let
    mkOverlays = system: [
      (final: prev: {
        # programs from nixos-unstable
        jujutsu = inputs.nixpkgs-unstable.legacyPackages.${system}.jujutsu;
        fzf = inputs.nixpkgs-unstable.legacyPackages.${system}.fzf;
        neovim = inputs.nixpkgs-unstable.legacyPackages.${system}.neovim;
        mergiraf = inputs.nixpkgs-unstable.legacyPackages.${system}.mergiraf;
        evil-helix = inputs.nixpkgs-unstable.legacyPackages.${system}.evil-helix;

        testsearch = inputs.testsearch.packages.${system}.default;
        listprojects = inputs.listprojects.packages.${system}.default;
        keymapp = final.callPackage ./derivations/keymapp {pkgs = final;};
        codelldb = final.callPackage ./derivations/codelldb {};
        localdocs = final.callPackage ./derivations/localdocs {};
        notify-wrapper = final.callPackage ./derivations/notify-wrapper {};
        monaspace = final.callPackage ./derivations/monaspace {};
        ansi = final.callPackage ./derivations/ansi {};
        wally = final.callPackage ./derivations/wally {};
        zizmor = final.callPackage ./derivations/zizmor {};
        cert-info = cert-info.packages.${system}.default;
        simpleproxy = inputs.simpleproxy.packages.${system}.default;
        gh-repo-url = final.callPackage ./derivations/gh-repo-url {};
        gh-pr-url = final.callPackage ./derivations/gh-pr-url {};
        gh-rebase-pr = final.callPackage ./derivations/gh-rebase-pr {};
        wlman = final.callPackage ./derivations/wlman {};
        check-certificate-revocation = final.callPackage ./derivations/check-certificate-revocation {};
        logtimes = inputs.logtimes.packages.${system}.default;
        notestools = inputs.notestools.packages.${system}.default;
        # https://kokada.capivaras.dev/blog/quick-bits-realise-nix-symlinks/
        realise-symlink = final.writeShellApplication {
          name = "realise-symlink";
          runtimeInputs = with final; [coreutils];
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
            # backup home-manager files that already exist
            home-manager.backupFileExtension = "backup";
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

        mkDarwinConfiguration = {hostname}:
          darwin.lib.darwinSystem {
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
                # backup home-manager files that already exist
                home-manager.backupFileExtension = "backup";
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
                };
              }
            ];
          };
      in {
        mba = mkDarwinConfiguration {hostname = "mba";};
        mm = mkDarwinConfiguration {hostname = "mm";};
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
      }
    );

    modules.modules = {
      # common module to configure nix
      nix = {...}: {
        nix.registry.nixpkgs.flake = nixpkgs;
        nixpkgs.flake = {
          setFlakeRegistry = false;
          setNixPath = false;
        };
        # set the system "nixpkgs" to the nixpkgs defined in this flake
        # https://dataswamp.org/~solene/2022-07-20-nixos-flakes-command-sync-with-system.html#_nix-shell_vs_nix_shell
        nix.nixPath = ["nixpkgs=/etc/channels/nixpkgs"];
        environment.etc."channels/nixpkgs".source = nixpkgs.outPath;
      };
      nixos = {name ? throw "No module name provided"}: import ./system/nixos/${name}/configuration.nix;
      darwin = {name ? throw "No module name provided"}: import ./system/darwin/${name}/configuration.nix;
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
