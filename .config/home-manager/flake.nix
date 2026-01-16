{
  description = "Home Manager Flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    optnix.url = "github:water-sucks/optnix";
    optnix.inputs.nixpkgs.follows = "nixpkgs";
    # nixvim.url = "github:nix-community/nixvim";
    # nixvim.inputs.nixpkgs.follows = "nixpkgs";
    nixosFlake.url = "path:/Users/aye011/dev/nixos";

  };

  outputs =
    {
      nixpkgs,
      home-manager,
      optnix,
      nixosFlake,
      ...
    }:
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
          {
            imports = [
              optnix.homeModules.optnix
            ];
          }
          (
            { options, config, ... }:
            let
              optnixLib = optnix.mkLib pkgs;
            in
            {
              programs.optnix = {
                enable = true;
                settings = {
                  scopes = let
                    # nixosConfigs = (builtins.getFlake "/Users/aye011/dev/nixos").nixosConfigurations;

                  in {

                    nixos = {
                      description = "nixos options";
                      options-list-file = optnixLib.mkOptionsList { inherit (nixosFlake.nixosConfigurations.nixos) options; };
                      # options-list-file = optnixLib.mkOptionsList { inherit (nixpkgs) options; };
                      evaluator = "nix eval \"/Users/aye011/dev/nixos/\"#nixosConfigurations.nixos.config.{{ .Option }}";
                    };

                    home-manager = {
                      description = "home-manager configuration for all systems";
                      options-list-file = optnixLib.mkOptionsList {
                        inherit options;
                        transform =
                          o:
                          o
                          // {
                            name = pkgs.lib.removePrefix "home-manager.users.${config.home.username}." o.name;
                          };
                      };
                      evaluator = "";
                    };
                  };
                };
              };

            }
          )

          # Set up nix registry with nixpkgs flakes.
          {
            nix.registry.nixpkgs.flake = nixpkgs;
            nixpkgs.config = {
              allowUnfree = true;
            };
          }

        ];
      };

      # Format with `nix fmt`
      formatter.${system} = pkgs.nixpkgs-fmt;
    };
}
