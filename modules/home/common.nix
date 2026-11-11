# Cross-platform shell/editor configuration.
{ pkgs, ... }:
{
  imports = [
    ../programs/zsh.nix
    ../programs/zed/zed.nix
  ];

  home.packages = [ pkgs.pi ];
}
