{
  description = "personal configs (NixOS workstation + nix-darwin macbook)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

    # Separate nixpkgs for darwin: nix-darwin's branch must match its nixpkgs
    # branch (nix-darwin-26.05 ←→ nixpkgs-26.05-darwin), which is a different
    # channel from the Linux nixos-26.05 above. home-manager keeps following
    # the Linux nixpkgs; nix-darwin follows this one.
    nixpkgs-darwin.url = "github:NixOS/nixpkgs/nixpkgs-26.05-darwin";

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/nix-darwin-26.05";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };
  };
  
  
  outputs = { self, nixpkgs, nixpkgs-darwin, home-manager, nix-darwin, ... }@inputs: {

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

    darwinConfigurations.macbook = nix-darwin.lib.darwinSystem {
      system = "x86_64-darwin";
      modules = [
        ./hosts/macbook/default.nix
        home-manager.darwinModules.home-manager
        ./modules/home-darwin.nix
      ];
    };
  };
}
