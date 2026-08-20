{ pi, ... }:

let
  overlay = pi.overlays.default;
in
{
  nixpkgs.overlays = [ overlay ];

  # Home Manager uses its own package set unless useGlobalPkgs is enabled.
  home-manager.sharedModules = [
    { nixpkgs.overlays = [ overlay ]; }
  ];
}
