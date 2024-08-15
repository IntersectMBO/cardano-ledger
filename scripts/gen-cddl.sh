#!/usr/bin/env bash

set -euo pipefail

eras=("shelley" "conway")

for era in ${eras[@]}; do

  changed=$(git diff --name-only origin/master -- 'eras/${era}/impl/testlib/Test/Cardano/Ledger/${era^}/CDDL.hs')

  if [[ -n "$changed" ]]; then
    cabal run cardano-ledger-$era:exe:huddle-cddl
  fi

done
