#!/usr/bin/env bash

set -euo pipefail

# Update a source-repository-package stanza in cabal.project
#
# Usage: update-srp REPO REF
#
# REPO can be any regex that matches the repo URL
# REF can be any reference (eg a branch name or tag)

REPO=$1
REF=$2

URL=$(sed -n -e "\#location:.*$REPO#s#^ *location: *##p" cabal.project)

if [[ -z $URL ]]
then
  echo "There's no srp for $REPO in cabal.project" >&2
  exit 1
fi

PREFETCH=$(nix-prefetch-git --quiet "$URL" "$REF")

sed -i -e "
  \#location:.*$REPO#,/^$/{
    s#tag:.*#tag: $(jq -r .rev <<<"$PREFETCH")#
    s#--sha256:.*#--sha256: $(jq -r .hash <<<"$PREFETCH")#
  }" cabal.project
