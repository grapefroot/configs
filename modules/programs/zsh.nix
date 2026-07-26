{...}:
{
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;       # verified: modules/programs/zsh/default.nix:194
    syntaxHighlighting.enable = true;    # verified: modules/programs/zsh/default.nix:40
    oh-my-zsh = {
      enable = true;
      theme = "lambda";
      plugins = [ "git" "sudo" ];        # omz builtin plugin names
    };
    history = {
      size = 10000;                      # in-memory
      save = 10000;                      # on disk
      ignoreDups = true;
    };
  };
}
