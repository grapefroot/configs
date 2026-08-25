{ ... }:

let
  overlay = final: prev: {
    treesheets = final.callPackage ./programs/treesheets/package.nix {
      treesheets = prev.treesheets;
    };
  };
in
{
  nixpkgs.overlays = [ overlay ];

  # Home Manager uses its own package set unless useGlobalPkgs is enabled.
  home-manager.sharedModules = [
    { nixpkgs.overlays = [ overlay ]; }
  ];
}
