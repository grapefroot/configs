# Intel MacBook.
#   sudo darwin-rebuild switch --flake ~/configs#macbook

{
  nixpkgs.hostPlatform = "x86_64-darwin";
  networking.hostName = "MacBook-Pro-user";
}
