# microvm.nix host setup for the workstation.
#
# MicroVMs themselves are declared under `microvm.vms`; each VM imports
# ./microvm-firecracker.nix to select the firecracker hypervisor. A
# ready-to-adapt example is commented out below.
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
  # command (installed system-wide by the host module) needs to create,
  # update and run VMs.
  users.groups.kvm = { };

  users.users.focus.extraGroups      = [ "kvm" ];
  users.users.grapefroot.extraGroups = [ "kvm" ];
  users.users.admin.extraGroups      = [ "kvm" ];

  # The host module enables itself by default on import
  microvm.host.enable = true;

  # --- Declarative MicroVMs ------------------------------------------------
  # Define firecracker MicroVMs here. Each VM's `config` is an ordinary
  # NixOS configuration that imports ./microvm-firecracker.nix. Uncomment
  # and adapt to add a VM:
  #
  # microvm.vms.my-vm = {
  #   autostart = false;          # start manually: systemctl start microvm@my-vm
  #   config = {
  #     imports = [ ./microvm-firecracker.nix ];
  #     microvm.mem = 512;
  #     microvm.vsock.cid = 3;            # enable AF_VSOCK
  #     microvm.vsock.ssh.enable = true;  # sshd on vsock::22; reach via `microvm -s my-vm`
  #     users.users.root.initialPassword = "";
  #     system.stateVersion = "26.05";
  #   };
  # };
  #
  # To manage a VM defined in another flake imperatively instead:
  #   microvm -f /absolute/path/to/flake -c my-vm
}
