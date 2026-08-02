{ pkgs, ... }:
{
  users.users."grapefroot" = {
    isNormalUser = true;
    description = "grapefroot";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.zsh;
  };

  users.users.admin = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.zsh;
  };

  users.users.focus = {
    isNormalUser = true;
    extraGroups = [];
    shell = pkgs.zsh;
  };
}
