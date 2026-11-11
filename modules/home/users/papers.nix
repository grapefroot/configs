{ pkgs, ... }:
{
  imports = [ ../linux-desktop.nix ];

  home.username = "papers";
  home.homeDirectory = "/home/papers";
  home.stateVersion = "26.05";

  home.packages = [ pkgs.treesheets ];
  xdg.configFile."hypr/hyprland.lua".source = ../../../home/hypr/papers.lua;
}
