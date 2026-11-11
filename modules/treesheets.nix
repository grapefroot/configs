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
}
