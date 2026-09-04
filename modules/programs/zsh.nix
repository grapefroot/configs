{...}:
{
  programs.zsh = {
    enable = true;
    autosuggestion.enable = false;      # disable inline history suggestions
    syntaxHighlighting.enable = true;   # verified: modules/programs/zsh/default.nix:40
    oh-my-zsh = {
      enable = true;
      theme = "lambda";
      plugins = [ "git" "sudo" ];        # omz builtin plugin names
    };
    history = {
      append = true;                     # preserve entries from parallel shells
      size = 50000;                      # in-memory
      save = 50000;                      # on disk
      ignoreDups = true;
    };
  };
}
