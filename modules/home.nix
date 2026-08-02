{pkgs, ...}:
{

  programs.zsh.enable = true;  # NixOS-level: generate /etc/zsh* so login works

  home-manager.useUserPackages = true;
  home-manager.backupFileExtension = "hm-backup";

  home-manager.users.focus = {

    imports = [
      ./programs/zsh.nix
    ];

    home.packages = with pkgs; [
      git
      tmux
      foot
      zed-editor
      pi-coding-agent
    ];

    home.username = "focus";
    home.homeDirectory = "/home/focus";
    home.stateVersion = "26.05";
    home.file.".tmux.conf".source = ../home/.tmux.conf;
    home.file.".config/tmuxinator".source = ../home/tmuxinator;
    home.file.".config/hypr/hyprland.lua".source = ../home/hypr/focus.lua;

    home.pointerCursor = {
      package = pkgs.adwaita-icon-theme;
      name = "Adwaita";
      size = 24;
      hyprcursor.enable = true;
    };
  };

  home-manager.users.grapefroot = {
    imports = [
      ./programs/zsh.nix
    ];

    home.packages = with pkgs; [
      git
      tmux
      foot
      firefox
      zed-editor
      vim
      git
      pi-coding-agent
      bemenu
      playerctl
    ];

    home.username = "grapefroot";
    home.homeDirectory = "/home/grapefroot";
    home.stateVersion = "26.05";
    home.file.".tmux.conf".source = ../home/.tmux.conf;

    home.pointerCursor = {
      package = pkgs.adwaita-icon-theme;
      name = "Adwaita";
      size = 24;
      hyprcursor.enable = true;
    };
  };

  home-manager.users.admin = {
    imports = [
      ./programs/zsh.nix
    ];

    home.packages = with pkgs; [
      git
      tmux
      foot
      firefox
      zed-editor
      vim
      git
      pi-coding-agent
      bemenu
      playerctl
    ];

    home.username = "admin";
    home.homeDirectory = "/home/admin";
    home.stateVersion = "26.05";
    home.file.".tmux.conf".source = ../home/.tmux.conf;

    home.pointerCursor = {
      package = pkgs.adwaita-icon-theme;
      name = "Adwaita";
      size = 24;
      hyprcursor.enable = true;
    };
  };
}
