{
  description = "PUHSU's Nix Configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # nixpkgs.url = "github:nixos/nixpkgs/7ad7702fa8b0d409aaf83eba8be1479b97823cdc";

    nix-darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Tried to live with the bleeding edge emacs, not for me right now, encountered
    # bugs that were annoying and hard to fix
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nix-darwin, home-manager, ...}: {

    darwinConfigurations."irubachev-osx" = nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        {
          users.users.irubachev.home = "/Users/irubachev";
          ids.gids.nixbld = 30000;
          nix.settings = {
            experimental-features = "nix-command flakes";
            substituters = [
              "https://cache.nixos.org"
              "https://nix-community.cachix.org"
              "https://devenv.cachix.org"
            ];
            trusted-public-keys = [
              "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
              "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
              "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
            ];
          };

          system.stateVersion = 6;
        }
        home-manager.darwinModules.home-manager
        {
          nixpkgs.overlays = [
            self.inputs.emacs-overlay.overlays.default
          ];
          home-manager.useGlobalPkgs = true;
          home-manager.users."irubachev" = import ./home.nix;
        }
        {
          programs.fish.enable = true;
        }
        {
          homebrew = {
            enable = true;
            casks = [
              "ghostty"
              "firefox"
              "telegram"
            ];
          };
        }
      ];
    };

    # MacOS Laptop Configurations
    homeConfigurations."mac" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.aarch64-darwin;
      
      modules = [ 
        {
          home.homeDirectory = "/Users/irubachev";
          nixpkgs.overlays = [
            self.inputs.emacs-overlay.overlays.default
          ];
        }
	      ./home.nix
	    ];
    };
    
    # Non NixOS dev server home configs
    homeConfigurations."linux" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules = [ 
        {
          home.homeDirectory = "/home/irubachev";
          nixpkgs.overlays = [
            self.inputs.emacs-overlay.overlays.default
          ];
        }
	      ./home.nix
	    ];          
    };

    # NixOS server config
    nixosConfigurations."bubu" = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./bubu.nix
      ];
    };

  };
}
