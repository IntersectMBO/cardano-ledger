#!/usr/bin/env bash

set -euo pipefail

# Install doctest's Cabal integration
cabal install doctest --flag cabal-doctest --ignore-project --overwrite-policy=always

# Ensure the cabal-doctest executable can be found
PATH=$(cabal path --installdir):$PATH

# Ensure other scripts (eg cabal-targets.hs) can be found
case "$0" in
  */*) PATH=$(dirname "$(which "$0")"):$PATH;;
esac

# Specify additional arguments needed for specific modules
EXTRA_ARGS=$(mktemp)
trap 'rm -f "$EXTRA_ARGS"' EXIT
cat <<EOF >"$EXTRA_ARGS"
cardano-ledger-api:lib:cardano-ledger-api --build-depends=cardano-ledger-babbage:testlib --build-depends=cardano-ledger-babbage-test
EOF

# Run the doctests for some or all packages
cabal-targets.hs "$@" |
sort | join -t' ' -a1 -j1 - "$EXTRA_ARGS" |
while read -ra ARGS
do
  cabal doctest --repl-options='-w -Wdefault' "${ARGS[@]}"
done
