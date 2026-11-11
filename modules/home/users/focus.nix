{ pkgs, ... }:
{
  imports = [ ../workstation-development.nix ];

  home.username = "focus";
  home.homeDirectory = "/home/focus";
  home.stateVersion = "26.05";

  home.packages = with pkgs; [
    docker
    docker-compose
  ];

  xdg.configFile = {
    "tmuxinator".source = ../../../home/tmuxinator;
    "hypr/hyprland.lua".source = ../../../home/hypr/focus.lua;
  };
}
