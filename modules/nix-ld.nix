# nix-ld: run unpatched dynamic-linked binaries (PyPI wheels, IDEs,
# proprietary downloads, ...) on NixOS.
#
{ pkgs, ... }:
{
  programs.nix-ld = {
    enable = true;
    libraries = with pkgs; [
      stdenv.cc.cc.lib
    ];
  };
}
