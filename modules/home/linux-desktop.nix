{ pkgs, ... }:
{
  imports = [ ./common.nix ];

  home.packages = with pkgs; [
    foot
    sioyek
  ];
  home.file.".tmux.conf".source = ../../home/.tmux.conf;

  home.pointerCursor = {
    package = pkgs.adwaita-icon-theme;
    name = "Adwaita";
    size = 24;
    hyprcursor.enable = true;
  };
}
