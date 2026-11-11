# Shared by the three development accounts, not the papers account.
{ pkgs, config, ... }:
{
  imports = [ ./linux-desktop.nix ];

  home.packages = with pkgs; [
    git
    tmux
    tmuxinator
    uv
    ruff
    package-version-server
  ];

  home.file."code".source = config.lib.file.mkOutOfStoreSymlink "/srv/shared/code";
}
