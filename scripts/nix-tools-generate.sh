#!/usr/bin/env bash
set -euo pipefail
cd $(git rev-parse --show-toplevel)

# until https://github.com/NixOS/nix/issues/1930 is fixed, we'll need
# this work around.
nix build '(import ./lib.nix).nix-tools.regeneratePackages' --no-link
$(nix path-info '(import ./lib.nix).nix-tools.regeneratePackages')
