{
  description = "workstation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  
  
  outputs = { self, nixpkgs, home-manager, ... }@inputs: {

    nixosConfigurations = {

      workstation = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./hosts/workstation/default.nix
          ./hosts/workstation/hardware-configuration.nix
          ./modules/users.nix
          ./modules/shared-files.nix
          ./modules/nix-restrict.nix
          home-manager.nixosModules.home-manager
          ./modules/home.nix
        ];
      };
    };
  };
}
