{ nixpkgs-unstable, ... }:
{
  nixpkgs.overlays = [
    (final: _prev: {
      unstable = import nixpkgs-unstable {
        system = final.stdenv.hostPlatform.system;
        config.allowUnfree = true;
      };
    })
  ];

  programs.zsh.enable = true;

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    backupFileExtension = "hm-backup";

    users = {
      focus = import ./home/users/focus.nix;
      grapefroot = import ./home/users/grapefroot.nix;
      admin = import ./home/users/admin.nix;
      papers = import ./home/users/papers.nix;
    };
  };
}
