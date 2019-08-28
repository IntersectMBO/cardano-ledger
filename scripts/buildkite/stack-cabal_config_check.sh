#!/bin/sh
set -euo pipefail

# Check that functions are defined.
type cardano-repo-tool &>/dev/null || { echo "cardano-repo-tool not found."; exit 1; }
type git               &>/dev/null || { echo "git not found."; exit 1; }

# Update cabal.project file from stack.yaml
# If they're out of sync, then the diff should be non-empty
cardano-repo-tool update-cabal-project

# This command exits 1 if the diff is non-empty, and 0 if the
# diff is empty.
git diff --exit-code
