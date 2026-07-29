{ pkgs, ... }:
{
  users.users."grapefroot" = {
    isNormalUser = true;
    description = "grapefroot";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  users.users.admin = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" ];
  };

  users.users.focus = {
    isNormalUser = true;
    extraGroups = [];
    shell = pkgs.zsh;
  };
}
