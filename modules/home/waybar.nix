{ config, lib, ... }:
{
  programs.waybar = {
    enable = true;
    systemd = {
      enable = true;
      targets = [ "graphical-session.target" ];
    };
    style = ../../home/waybar/style.css;
  };

  # Keep JSONC as a source file so comments remain supported.
  xdg.configFile."waybar/config.jsonc".source = ../../home/waybar/config.jsonc;

  systemd.user.services.waybar = {
    Unit = {
      # Preserve the existing session lifecycle, without adding tray.target.
      PartOf = lib.mkForce [ "graphical-session.target" ];
      X-Reload-Triggers = [ "${config.xdg.configFile."waybar/config.jsonc".source}" ];
    };
    Service.RestartSec = "2";
    Install.WantedBy = lib.mkForce [ "graphical-session.target" ];
  };
}
