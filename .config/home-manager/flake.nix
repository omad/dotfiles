{
    description = "Home Manager Flake";

    inputs = {
        nixpkgs.url = "nixpkgs/nixos-unstable";
        home-manager.url = "github:nix-community/home-manager/master";
        home-manager.inputs.nixpkgs.follows = "nixpkgs";
    };

    outputs = { nixpkgs, home-manager, ... }:
    let
        system = "x86_64-linux";
        pkgs = import nixpkgs {
            inherit system;
            config = {
                allowUnfree = true;
            };
        };

        lib = nixpkgs.lib;
    in {
        homeManagerConfigurations = {
            omad = home-manager.lib.homeManagerConfiguration {
                inherit system pkgs;
                username = "omad";
                homeDirectory = "/home/omad";
                configuration = {
                    imports = [
                        ./home.nix
                    ];
                };
            };
        };
    };
}
