#!/usr/bin/env bash

set -euo pipefail

echo "Generating plutus examples..."
cabal run plutus-preprocessor
echo "Regenerated plutus examples"

git diff --exit-code
