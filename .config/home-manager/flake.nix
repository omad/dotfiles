{
    description = "Home Manager Flake";

    inputs = {
        nixpkgs.url = "nixpkgs/nixos-unstable";
        home-manager.url = "github:nix-community/home-manager/master";
        home-manager.inputs.nixpkgs.follows = "nixpkgs";
#        martinvonz.url = "github:martinvonz/jj";
#        martinvonz.inputs.nixpkgs.follows = "nixpkgs";
    };

    outputs = { nixpkgs, home-manager, ... }:
    let
        system = "x86_64-linux";
        pkgs = nixpkgs.legacyPackages.${system};
#        martinvonz_pkgs = martinvonz.packages.${system};
#        pkgs = import nixpkgs {
#            inherit system;
#            config = {
#                allowUnfree = true;
#            };
#        };

#        lib = nixpkgs.lib;
    in {
        homeConfigurations."omad" = home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [
                ./home.nix

                # Set up nix registry with nixpkgs flakes.
                ({ lib, ... }: {
                  nix.registry.nixpkgs.flake = nixpkgs;
                  nixpkgs.config = {
                    allowUnfree = true;
                  };
#                  home.packages = [
#                      martinvonz_pkgs.jujutsu
#                  ];
        #          nix.registry.nixpkgs-unstable.flake = nixpkgs-unstable;
                })

                ({ ... }: {
                })
            ];


        };
    };
}
