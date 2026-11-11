{ pi, ... }:

{
  # Expose the official Pi flake package as pkgs.pi.
  nixpkgs.overlays = [ pi.overlays.default ];
}
