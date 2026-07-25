{...}:
{

  home-manager.useUserPackages = true;
  home-manager.backupFileExtension = "hm-backup";

  home-manager.users.focus = {
    home.username = "focus";
    home.homeDirectory = "/home/focus";
    home.stateVersion = "26.05";
  };
}
