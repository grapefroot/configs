{pkgs, ...}:
{

  programs.zsh.enable = true;  # NixOS-level: generate /etc/zsh* so login works

  home-manager.useUserPackages = true;
  home-manager.backupFileExtension = "hm-backup";

  home-manager.users.focus = { pkgs, config, ... }: {

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
    home.file."code".source = config.lib.file.mkOutOfStoreSymlink "/srv/shared/code";
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

  home-manager.users.grapefroot = { pkgs, config, ... }: {
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
      darktable
    ];

    home.username = "grapefroot";
    home.homeDirectory = "/home/grapefroot";
    home.stateVersion = "26.05";
    home.file."code".source = config.lib.file.mkOutOfStoreSymlink "/srv/shared/code";
    home.file.".tmux.conf".source = ../home/.tmux.conf;

    home.pointerCursor = {
      package = pkgs.adwaita-icon-theme;
      name = "Adwaita";
      size = 24;
      hyprcursor.enable = true;
    };
  };

  home-manager.users.admin = { pkgs, config, ... }: {
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
    home.file."code".source = config.lib.file.mkOutOfStoreSymlink "/srv/shared/code";
    home.file.".tmux.conf".source = ../home/.tmux.conf;

    home.pointerCursor = {
      package = pkgs.adwaita-icon-theme;
      name = "Adwaita";
      size = 24;
      hyprcursor.enable = true;
    };
  };


  home-manager.users.papers = {

    imports = [
      ./programs/zsh.nix
    ];

    home.packages = with pkgs; [
      tmux
      foot
      zed-editor
      pi-coding-agent
      sioyek
    ];

    home.username = "papers";
    home.homeDirectory = "/home/papers";
    home.stateVersion = "26.05";
    home.file.".tmux.conf".source = ../home/.tmux.conf;
    home.file.".config/hypr/hyprland.lua".source = ../home/hypr/papers.lua;

    home.pointerCursor = {
      package = pkgs.adwaita-icon-theme;
      name = "Adwaita";
      size = 24;
      hyprcursor.enable = true;
    };
  };

}
