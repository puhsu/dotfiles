{
  description = "Home Manager configuration of puhsu";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, flake-utils, ...}: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      packages = {
        homeConfigurations."dotfiles" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;

          modules = [ 
	          ./home.nix
	        ];
        };
      };
    }
  );
}
