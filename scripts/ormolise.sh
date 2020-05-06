#!/usr/bin/env nix-shell
#! nix-shell ../shell.nix -i bash

set -euo pipefail

ormolu -m inplace $(git ls-files -- 'shelley/chain-and-ledger/executable-spec/*.hs' | grep -v Setup.hs)
