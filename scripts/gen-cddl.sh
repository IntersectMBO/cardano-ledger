#!/usr/bin/env bash

set -euo pipefail

changed=$(git diff --name-only origin/master -- 'eras/conway/impl/testlib/Test/Cardano/Ledger/Conway/CDDL.hs')

if [[ -n "$changed" ]]; then
  cabal run cardano-cddl
fi
