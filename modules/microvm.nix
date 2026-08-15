# microvm.nix host setup for the workstation.
#
# Docs:
#   https://microvm-nix.github.io/microvm.nix/host.html
#   https://microvm-nix.github.io/microvm.nix/declarative.html

{ ... }:

{
  # The microvm host module runs every `microvm@*.service` as the system
  # user `microvm` (group `kvm`) and keeps all VM state under
  # /var/lib/microvms, owned microvm:kvm with mode 0775. The `kvm` group
  # also owns /dev/kvm, so membership grants both /dev/kvm access and write
  # access to the MicroVM state directory — exactly what the `microvm`
  users.groups.kvm = { };

  users.users.focus.extraGroups      = [ "kvm" ];
  users.users.grapefroot.extraGroups = [ "kvm" ];
  users.users.admin.extraGroups      = [ "kvm" ];

  # The host module enables itself by default on import
  microvm.host.enable = true;

  # qemu exposes VSOCK natively via vhost-vsock (needs /dev/vsock on the
  # host), unlike firecracker/cloud-hypervisor which use a Unix socket.
  boot.kernelModules = [ "vhost_vsock" ];

  # firecracker/cloud-hypervisor expose VSOCK as a Unix domain socket
  # (notify.vsock) created by the VMM process (user microvm, group kvm) with
  # the process umask. Default umask 022 → socket mode 0755, so group `kvm`
  # can read but not *connect* (connect needs write). UMask=0002 → mode 0775,
  systemd.services."microvm@".serviceConfig.UMask = "0002";

  # --- pi sandbox VM (qemu) -----------------------------------------------
  # Isolated NixOS running the pi coding agent. qemu (KVM) gives the same
  # kernel-level isolation as firecracker but supports virtiofs shares and
  # user networking, which an agent needs: /srv/shared/code is mounted
  # read-write at /code, and qemu slirp "user" networking gives outbound
  # internet (LLM API, git) with zero host bridge setup. Reach the guest
  # over native AF_VSOCK: `microvm -s pi` (after `systemctl start microvm@pi`).
  microvm.vms.pi = {
    autostart = false;          # start manually: sudo systemctl start microvm@pi
    config = { pkgs, ... }: {
      imports = [ ./microvm-qemu.nix ];
      microvm = {
        mem = 1024;            # node + agent context; tune down if you want
        vcpu = 2;
        vsock.cid = 3;
        vsock.ssh.enable = true;          # sshd on vsock::22; `microvm -s pi`
        interfaces = [{
          type = "user";                  # qemu slirp: outbound net, no host config
          id = "net0";
          mac = "02:00:00:00:00:02";
        }];
        shares = [{
          source = "/srv/shared/code";    # host code dir (the ~/code symlink target)
          mountPoint = "/code";
          tag = "code";
          proto = "virtiofs";
        }];
      };
      networking.useDHCP = false;
      systemd.network.enable = true;
      systemd.network.networks."10-lan" = {
        matchConfig.Type = "ether";
        networkConfig.DHCP = "yes";
      };
      environment.systemPackages = with pkgs; [
        pi-coding-agent   # bundles nodejs, ripgrep, fd, bash, python3
        git
        tmux
        vim
      ];
      # Guest root is PermitRootLogin=prohibit-password, so authorize a key.
      # Re-run `nix store prefetch-file <url>` if the published key changes.
      users.users.root.openssh.authorizedKeys.keyFiles = [
        (builtins.fetchurl {
          url = "https://github.com/grapefroot.keys";
          sha256 = "3IKn8Lj7elIljGql9MV4BsZe46jFnJT0WrA7elkH/xI=";
        })
      ];
      system.stateVersion = "26.05";
    };
  };

  # --- Declarative MicroVMs ------------------------------------------------
  # Define more MicroVMs here. Each VM's `config` is an ordinary NixOS
  # configuration that imports a hypervisor profile (./microvm-qemu.nix or
  # ./microvm-firecracker.nix). Uncomment and adapt to add a VM:
  #
  # microvm.vms.my-vm = {
  #   autostart = false;          # start manually: systemctl start microvm@my-vm
  #   config = { pkgs, ... }: {
  #     imports = [ ./microvm-qemu.nix ];
  #     microvm.mem = 512;
  #     microvm.vsock.cid = 4;            # enable AF_VSOCK; `microvm -s my-vm`
  #     microvm.vsock.ssh.enable = true;
  #     system.stateVersion = "26.05";
  #   };
  # };
  #
  # To manage a VM defined in another flake imperatively instead:
  #   microvm -f /absolute/path/to/flake -c my-vm
}
