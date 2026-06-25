#!/usr/bin/env bash

set -euo pipefail

eras=("shelley" "allegra" "mary" "alonzo" "babbage" "conway" "dijkstra")

for era in "${eras[@]}"; do

  echo "Generating cddl for $era..."
  cabal run "cardano-ledger-$era:exe:generate-cddl"
  echo "Regenerated ${era}.cddl"

done

git diff --exit-code
