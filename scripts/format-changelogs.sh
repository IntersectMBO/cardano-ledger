#!/usr/bin/env bash

set -euo pipefail

FILES=("$@")

if [[ ${#FILES[@]} -eq 0 ]]
then
  mapfile -t FILES < <(git ls-files '*CHANGELOG.md')
fi

clrt changelogs -i "${FILES[@]}"

git diff -s --exit-code "${FILES[@]}"
