{ pkgs, ... }:
{
  users.users."grapefroot" = {
    isNormalUser = true;
    description = "grapefroot";
    extraGroups = [ "networkmanager" "wheel" "papers" "shared" ];  # write access to /home/papers/papers
    shell = pkgs.zsh;
  };

  users.users.admin = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" "shared" ];
    shell = pkgs.zsh;
  };

  users.users.focus = {
    isNormalUser = true;
    extraGroups = [ "shared" ];
    shell = pkgs.zsh;
  };

  users.users.papers = {
    isNormalUser = true;
    extraGroups = [ "papers" ];  # read access to /home/papers/papers
    shell = pkgs.zsh;
  };

  # Shared group for the ~/papers directory (admin writes, papers reads)
  users.groups.papers = {};
}
