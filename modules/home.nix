{...}:
{

  programs.zsh.enable = true;  # NixOS-level: generate /etc/zsh* so login works

  home-manager.useUserPackages = true;
  home-manager.backupFileExtension = "hm-backup";

  home-manager.users.focus = {

    imports = [
      ./programs/zsh.nix
    ];

    home.username = "focus";
    home.homeDirectory = "/home/focus";
    home.stateVersion = "26.05";
    home.file.".tmux.conf".source = ../home/.tmux.conf;
    home.file.".config/tmuxinator".source = ../home/tmuxinator;
  };
}
