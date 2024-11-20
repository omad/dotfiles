{
  description = "Home Manager Flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, home-manager, nixvim, ... }:
    let
#      system = "x86_64-linux";
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      homeConfigurations."aye011" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./home.nix
          #          nixvim.homeManagerModules.nixvim

          ./k9s.nix


          # Set up nix registry with nixpkgs flakes.
          ({ lib, ... }: {
            nix.registry.nixpkgs.flake = nixpkgs;
            nixpkgs.config = {
              allowUnfree = true;
            };
          })

        ];
      };

      # Format with `nix fmt`
      formatter.${system} = pkgs.nixpkgs-fmt;
    };
}
