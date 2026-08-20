{
  description = "personal configs (NixOS workstation + nix-darwin macbook)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

    # Separate nixpkgs for darwin: nix-darwin's branch must match its nixpkgs
    # branch (nix-darwin-26.05 ←→ nixpkgs-26.05-darwin), which is a different
    # channel from the Linux nixos-26.05 above. home-manager keeps following
    # the Linux nixpkgs; nix-darwin follows this one.
    nixpkgs-darwin.url = "github:NixOS/nixpkgs/nixpkgs-26.05-darwin";

    pi.url = "github:lukasl-dev/pi.nix";

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/nix-darwin-26.05";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };

    # MicroVM.nix: run lightweight NixOS VMs on the workstation host.
    # `follows = "nixpkgs"` builds it against the same nixpkgs as the host.
    # Linux only — not referenced by the darwin/macbook configuration.
    microvm = {
      url = "github:microvm-nix/microvm.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  
  
  outputs = { self, nixpkgs, nixpkgs-darwin, home-manager, nix-darwin, microvm, pi, ... }: {

    nixosConfigurations = {

      workstation = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit pi; };
        modules = [
          ./modules/pi-coding-agent.nix
          ./hosts/workstation/default.nix
          ./hosts/workstation/hardware-configuration.nix
          ./modules/users.nix
          ./modules/shared-files.nix
          ./modules/nix-restrict.nix
          ./modules/keyd.nix
          ./modules/libinput-quirks.nix
          microvm.nixosModules.host
          ./modules/microvm.nix
          home-manager.nixosModules.home-manager
          ./modules/home.nix
        ];
      };
    };

    darwinConfigurations.macbook = nix-darwin.lib.darwinSystem {
      system = "x86_64-darwin";
      specialArgs = { inherit pi; };
      modules = [
        ./modules/pi-coding-agent.nix
        ./hosts/macbook/default.nix
        home-manager.darwinModules.home-manager
        ./modules/home-darwin.nix
      ];
    };
  };
}
