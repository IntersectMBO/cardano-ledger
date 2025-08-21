#!/usr/bin/env bash

set -euo pipefail

# Add a source-repository-package stanza to cabal.project
#
# Usage: add-srp URL REF [SUBDIRS]
#
# URL is the URL of the repo
# REF can be any reference (eg a branch name or tag)
# SUBDIRS is a list of subdirectories for the `subdir` field

URL=$1
REF=$2
SUBDIRS=("${@:3}")

if grep -q -e "^ *location:.*$URL" cabal.project
then
  echo "There's already an srp for $URL in cabal.project" >&2
  exit 1
fi

PREFETCH=$(nix-prefetch-git --quiet "$URL" "$REF")

cat <<EOF >>cabal.project

source-repository-package
  type: git
  location: $URL
  --sha256: $(jq -r .hash <<<"$PREFETCH")
  tag: $(jq -r .rev <<<"$PREFETCH")
EOF

if (( ${#SUBDIRS[@]} > 0 ))
then
  echo '  subdir:'
  for SUBDIR in "${SUBDIRS[@]}"
  do
    echo "    $SUBDIR"
  done
fi >>cabal.project
