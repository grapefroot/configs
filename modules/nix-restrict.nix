{...}:
{
  nix.settings.allowed-users = [ "root" "admin" "grapefroot" "focus"];
  nix.settings.trusted-users = [ "root" "admin" "grapefroot" ];
  nix.channel.enable = false;
}
