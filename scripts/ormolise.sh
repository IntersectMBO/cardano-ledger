#!/usr/bin/env bash

set -euo pipefail

ormolu -c -m inplace $(git ls-files -- 'shelley/chain-and-ledger/executable-spec/*.hs' 'shelley-ma/impl/*.hs' 'alonzo/*.hs' 'cardano-ledger-test/*.hs' | grep -v Setup.hs)

git diff --exit-code
