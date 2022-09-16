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

  outputs = { nixpkgs, darwin, flake-utils, home-manager, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      {
        homeConfigurations = {
          simon = home-manager.lib.homeManagerConfiguration {
            pkgs = pkgs;
            modules = [
              ./home.nix
            ];
          };
        };
        darwinConfigurations = {
          mba = darwin.lib.darwinSystem {
            system = "aarch64-darwin";
            modules = [
              ./configuration.nix
            ];
            inputs = { inherit darwin nixpkgs; };
          };
        };
      }
    );
}
