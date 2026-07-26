{...}:
{

  programs.zsh.enable = true;  # NixOS-level: generate /etc/zsh* so login works

  home-manager.useUserPackages = true;
  home-manager.backupFileExtension = "hm-backup";

  home-manager.users.focus = {
    home.username = "focus";
    home.homeDirectory = "/home/focus";
    home.stateVersion = "26.05";
    home.file.".tmux.conf".source = ../home/.tmux.conf;
    home.file.".config/tmuxinator".source = ../home/tmuxinator;
    programs.zsh = {
      enable = true;
      autosuggestion.enable = true;       # verified: modules/programs/zsh/default.nix:194
      syntaxHighlighting.enable = true;    # verified: modules/programs/zsh/default.nix:40
      oh-my-zsh = {
        enable = true;
        theme = "robbyrussell";
        plugins = [ "git" "sudo" ];        # omz builtin plugin names
      };
      history = {
        size = 10000;                      # in-memory
        save = 10000;                      # on disk
        ignoreDups = true;
      };
    };
  };
}
