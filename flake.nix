{
  description = "Home Manager configuration of puhsu";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, ...}: {
    # dev laptop config
    homeConfigurations."mac" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.aarch64-darwin;
      
      modules = [ 
        {
          home.homeDirectory = "/Users/irubachev";
        }
	      ./home.nix
	    ];
    };
    
    # dev non-nixos servers configs
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
