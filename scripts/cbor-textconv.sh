#!/usr/bin/env bash

set -euo pipefail

exec 2>&1 # Include stderr in the diff output, in case this is run from a GUI program

if ! cuddle format-cbor --fin binary --fout term "$1" | fourmolu -q --no-cabal
then
  echo '# (CBOR pretty-printing failed; hex dump follows)'
  od -An -tx1 -v "$1"
fi
