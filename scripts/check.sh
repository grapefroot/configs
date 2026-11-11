#!/usr/bin/env bash
# Evaluate configurations only: no system builds or activation.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."

files=(flake.nix)
while IFS= read -r -d '' file; do
  files+=("$file")
done < <(find hosts modules -type f -name '*.nix' -print0)

nix fmt --no-write-lock-file -- --check "${files[@]}"

for target in \
  nixosConfigurations.workstation.config.system.build.toplevel \
  darwinConfigurations.macbook.system \
  darwinConfigurations.macbook-air.system; do
  printf '\nEvaluating %s\n' "$target"
  nix eval --no-write-lock-file --raw ".#${target}.drvPath"
  printf '\n'
done
