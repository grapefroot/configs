# Apple Silicon MacBook Air.
#   sudo darwin-rebuild switch --flake ~/configs#macbook-air

{
  nixpkgs.hostPlatform = "aarch64-darwin";
  networking.hostName = "MacBook-Air-Glam";
}
