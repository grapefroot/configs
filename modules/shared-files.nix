{ pkgs, ...}:
{
  users.groups.shared = { };
  
  systemd.services.shared-dir = {
    description = "Shared directory at /srv/shared";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = pkgs.writeShellScript "setup-shared" ''
        ${pkgs.coreutils}/bin/mkdir -p /srv/shared
        ${pkgs.coreutils}/bin/chown root:shared /srv/shared
        ${pkgs.coreutils}/bin/chmod 2775 /srv/shared
        ${pkgs.acl}/bin/setfacl -m g:shared:rwx -m d:g:shared:rwx -m d:m:rwx /srv/shared
        ${pkgs.coreutils}/bin/mkdir -p /srv/shared/code
      ''; 
    };
  };
}
