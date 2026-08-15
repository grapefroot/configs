# Guest profile: run a MicroVM under the firecracker hypervisor.
#
# Import this from a MicroVM's `config` (see modules/microvm.nix for an
# example) to select firecracker. firecracker is a minimal, security-focused
# VMM; its main restriction is that it supports *no* virtiofs or 9p directory
# shares (https://microvm-nix.github.io/microvm.nix/). Because of this the VM
# cannot mount the host's /nix/store and instead boots from a self-contained
# store disk image (erofs by default; set `microvm.storeDiskType = "squashfs"`
# for a smaller image). AF_VSOCK (`microvm.vsock.cid`) is supported and is the
# simplest way to reach a VM without configuring a network interface.

{ ... }:

{
  microvm.hypervisor = "firecracker";
}
