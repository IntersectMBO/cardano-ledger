#!/usr/bin/env bash

set -euo pipefail

eras=("shelley" "allegra" "mary" "alonzo" "babbage" "conway")

for era in ${eras[@]}; do

  changed=$(git diff --name-only origin/master -- "eras/${era}/impl/testlib/Test/Cardano/Ledger/${era^}/CDDL.hs" | xargs)

  if [[ -n "$changed" ]]; then
    echo "Generating cddl for $era..."
    cabal run cardano-ledger-$era:exe:huddle-cddl
    echo "Regenerated ${era}.cddl"
  fi

done
