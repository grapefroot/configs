# Guest profile: run a MicroVM under the qemu hypervisor.
#
# qemu (via KVM) is the general-purpose VMM. Same kernel-level isolation as
# firecracker (separate guest kernel; the guest cannot touch the host except
# through explicitly declared shares), but — unlike firecracker — it supports
# virtiofs/9p directory shares and user (slirp) networking. That is what an
# interactive or agent workload needs: a mounted code directory and outbound
# internet for API/git access with no host bridge configuration.
#
# VSOCK is exposed natively (vhost-vsock → /dev/vsock on the host), so
# `microvm -s <name>` reaches the guest directly.

{ ... }:

{
  microvm.hypervisor = "qemu";
}
