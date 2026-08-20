{ pkgs, ... }:

{
  programs.zed-editor = {
    enable = true;

    # The Mac app is installed independently. Home Manager only merges the
    # shared settings there; on NixOS it also installs Zed.
    package = if pkgs.stdenv.isDarwin then null else pkgs.zed-editor;

    # Merge these checked-in defaults into Zed's writable configuration so
    # preferences can still be changed interactively from the editor.
    userSettings = builtins.fromJSON (builtins.readFile ./shared-settings.json);
    userKeymaps = builtins.fromJSON (builtins.readFile ./keymap.json);
    userTasks = builtins.fromJSON (builtins.readFile ./tasks.json);
  };
}
