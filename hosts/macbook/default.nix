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
  # Match current hostname (no change to your machine's name).
  networking.hostName = "MacBook-Pro-user";

  nix.enable = true;
  # Preserve the experimental features already in use.
  nix.settings.experimental-features = [ "nix-command" "flakes" "fetch-tree" ];
  nix.settings.trusted-users = [ "root" "grapefroot" ];

  # Packages migrated from the user nix profile (`nix profile list`).
  # These now live in the system profile at /run/current-system/sw/bin,
  # managed declaratively here instead of imperatively via `nix profile`.
  environment.systemPackages = with pkgs; [
    # --- CLI utilities ---
    bat
    fd
    ripgrep
    jq
    tmux
    tmuxinator
    gh
    jless
    television

    # --- Languages / dev tools ---
    go
    nodejs_24
    uv
    cmake
    bison
    doxygen

    # --- Media ---
    ffmpeg_7-headless
    gallery-dl
    yt-dlp
    hugo

    # --- Cloud / containers ---
    awscli2
    s3cmd
    docker

    # --- Docs ---
    asciidoc
    docutils

    # --- Libraries (kept for dev/linking, as in the old profile) ---
    openssl
    zlib
    libpng
    pcre2
    xz
    gobject-introspection
  ];

  # NOTE: the nix.linux-builder has been removed — the microvm experiment is
  # done on this Mac (moved to a Linux box where firecracker/KVM work natively).
  # To re-enable it later, add back:
  #   nix.linux-builder.enable = true;
  #   nix.linux-builder.config = { virtualisation.cores = lib.mkForce 2; };
  #   launchd.daemons.linux-builder.serviceConfig = {
  #     KeepAlive = lib.mkForce false; RunAtLoad = lib.mkForce false;
  #   };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;
}
