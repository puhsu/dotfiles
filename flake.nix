{
  description = "PUHSU's Nix Configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
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

    emacs-lsp-booster = {
      url = "github:slotThe/emacs-lsp-booster-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ...}: {
    
    home-manager.useGlobalPkgs = true;

    # MacOS Laptop Configurations
    homeConfigurations."mac" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.aarch64-darwin;
      
      modules = [ 
        {
          home.homeDirectory = "/Users/irubachev";
          nixpkgs.overlays = [
            self.inputs.emacs-overlay.overlays.default
            self.inputs.emacs-lsp-booster.overlays.default 
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
