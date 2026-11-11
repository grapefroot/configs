{ pkgs, ... }:
{
  imports = [ ../workstation-development.nix ];

  home.username = "admin";
  home.homeDirectory = "/home/admin";
  home.stateVersion = "26.05";

  home.packages = with pkgs; [
    firefox
    vim
    bemenu
    playerctl
  ];
}
