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
        homeConfigurations = {
            omad = home-manager.lib.homeManagerConfiguration {
                pkgs = nixpkgs.legacyPackages.${system};
                modules = [
                    ./home.nix
                    {
                        home = {
                            username = "omad";
                        };
                    }
                ];
            };
        };
    };
}
