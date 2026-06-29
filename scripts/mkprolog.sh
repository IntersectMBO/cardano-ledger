#!/usr/bin/env bash

set -euo pipefail

HADDOCKS_DIR=${1:-haddocks}
PROLOG_FILE=${2:-scripts/prolog}

exec >"$PROLOG_FILE" # Write all stdout to $PROLOG_FILE from here on

cd "$HADDOCKS_DIR"

cat << EOF
= Cardano Ledger Repository Documentation

== Benchmarks

* [Charts](dev/bench/) showing historical trends

== Package Documentation

[Skip to module list](#module-list)

This page contains Haskell documentation of:

EOF

find -- * -maxdepth 0 -type d | while read -r dir; do
  echo "* __[$dir](${dir//:/%3A}/index.html)__"
done
