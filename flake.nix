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
  };

  outputs = { nixpkgs, darwin, flake-utils, home-manager, ... }: {
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem
      {
        system = "x86_64-linux";
        modules = [
          ./system/nixos/nixos/configuration.nix
        ];
      } //
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
      in
      {
        homeConfigurations = {
          simon = home-manager.lib.homeManagerConfiguration {
            pkgs = pkgs;
            modules = [
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
  };
}
