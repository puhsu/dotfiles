{
  description = "Home Manager configuration of puhsu";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, ...}: {
    # dev laptop config
    homeConfigurations."mac" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.aarch64-darwin;
      modules = [ 
	      ./home.nix
        {  
          home.homeDirectory = "/Users/irubachev";
        }
	    ];
    };
    
    # dev servers config
    homeConfigurations."linux" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules = [ 
	      ./home.nix
        {  
          home.homeDirectory = "/home/irubachev";
        }
	    ];          
    };

  };
}
