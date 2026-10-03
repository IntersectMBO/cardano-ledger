#!/usr/bin/env bash

set -euo pipefail
shopt -s nullglob

# Run a package's tests with coverage and generate the hpc report for all
# modules of all packages in this project, including other-modules, which
# cabal's own report omits.
#
# Usage: coverage.sh PACKAGE [CABAL_TEST_FLAGS..]

PACKAGE=${1:?Usage: coverage.sh PACKAGE [CABAL_TEST_FLAGS..]}
shift

cabal test "$PACKAGE" --enable-coverage "$@"

# Absolute paths, because hpc resolves --hpcdir and --destdir relative to --srcdir
TIX=$(find "$PWD/dist-newstyle" -type f -path "*/$PACKAGE-[0-9]*/hpc/vanilla/tix/*.tix" -printf '%T@ %p\n' \
  | sort -rn | head -n1 | cut -d' ' -f2-)
GHC_DIR=$(dirname "${TIX%/t/*}")
DEST_DIR=${TIX%/tix/*}/html-full

# Mix dirs of every locally built library and sublibrary
MIX_DIRS=(
  "$GHC_DIR"/*/build/extra-compilation-artifacts/hpc/vanilla/mix
  "$GHC_DIR"/*/l/*/build/*/extra-compilation-artifacts/hpc/vanilla/mix
)
HPC_DIRS=$(printf -- '--hpcdir=%s\n' "${MIX_DIRS[@]}")
SRC_DIRS=$(git ls-files '*.cabal' | sed 's|/[^/]*$||; s/^/--srcdir=/' | sort -u)
INCLUDES=$(find "${MIX_DIRS[@]}" -name '*.mix' ! -name 'Paths_*' -printf '%f\n' \
  | sed 's/\.mix$//; s/^/--include=/' | sort -u)

# shellcheck disable=SC2086
hpc markup "$TIX" --destdir="$DEST_DIR" $HPC_DIRS $SRC_DIRS $INCLUDES >/dev/null

echo "Coverage report: $DEST_DIR/hpc_index.html"
