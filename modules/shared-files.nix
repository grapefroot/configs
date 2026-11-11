{
  users.groups.shared = { };

  # Permanent directories: no age/cleanup and no recursive permission changes.
  # a+ merges ACL entries, matching the previous setfacl -m behavior.
  # https://www.freedesktop.org/software/systemd/man/latest/tmpfiles.d.html
  systemd.tmpfiles.rules = [
    "d /srv/shared 2775 root shared - -"
    "d /srv/shared/code 2775 root shared - -"
    "a+ /srv/shared - - - - g:shared:rwx,m::rwx,d:g:shared:rwx,d:m::rwx"
    "a+ /srv/shared/code - - - - g:shared:rwx,m::rwx,d:g:shared:rwx,d:m::rwx"
  ];
}
