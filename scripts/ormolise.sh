#!/usr/bin/env nix-shell
#! nix-shell ../shell.nix -i bash

set -euo pipefail

ormolu -c -m inplace $(git ls-files -- 'shelley/chain-and-ledger/executable-spec/*.hs' 'shelley-ma/impl/*.hs' | grep -v Setup.hs)

git diff --exit-code
