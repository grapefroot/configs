# nix-darwin configuration for the MacBook (Intel, x86_64-darwin).
#
# Applied with:
#   sudo darwin-rebuild switch --flake ~/configs#macbook
#
# Note: nix-darwin requires its nixpkgs branch to match (nix-darwin-26.05 ←→
# nixpkgs-26.05-darwin), so this config is evaluated against the separate
# `nixpkgs-darwin` input declared in flake.nix, NOT the Linux `nixpkgs`
# input used by the workstation. `pkgs` here resolves to that darwin set.

{ config, pkgs, lib, ... }:

{
  networking.hostName = "MacBook-Pro-user";

  nix.enable = true;
  nix.settings.experimental-features = [ "nix-command" "flakes" "fetch-tree" ];
  nix.settings.trusted-users = [ "root" "grapefroot" ];

  users.users.grapefroot.home = "/Users/grapefroot";

  #   nix.linux-builder.enable = true;
  #   nix.linux-builder.config = { virtualisation.cores = lib.mkForce 2; };
  #   launchd.daemons.linux-builder.serviceConfig = {
  #     KeepAlive = lib.mkForce false; RunAtLoad = lib.mkForce false;
  #   };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;
}
