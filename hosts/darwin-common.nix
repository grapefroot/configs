# Shared macOS settings. Packages and shell configuration live in
# modules/home-darwin.nix; each host supplies its platform and hostname.
# The flake uses nixpkgs-darwin to match the nix-darwin release.
{
  nix.enable = true;
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
    "fetch-tree"
  ];
  nix.settings.trusted-users = [
    "root"
    "grapefroot"
  ];

  users.users.grapefroot.home = "/Users/grapefroot";

  # Compatibility baseline; consult `darwin-rebuild changelog` before changing.
  system.stateVersion = 6;
}
