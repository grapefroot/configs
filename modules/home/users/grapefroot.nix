{ pkgs, ... }:
{
  imports = [
    ../workstation-development.nix
    ../waybar.nix
  ];

  home.username = "grapefroot";
  home.homeDirectory = "/home/grapefroot";
  home.stateVersion = "26.05";

  home.packages = with pkgs; [
    firefox
    vim
    bemenu
    playerctl
    darktable
    gphoto2
    unstable.claude-code
    treesheets
    docker
    docker-compose
    tailscale
  ];

  xdg.configFile = {
    "tmuxinator".source = ../../../home/tmuxinator;
    "hypr/hyprland.lua".source = ../../../home/hypr/grapefroot.lua;
  };
}
