#!/usr/bin/env bash

set -euo pipefail

for f in $(git ls-files -- '*.cabal'); do cabal format $f; done

git diff --exit-code
