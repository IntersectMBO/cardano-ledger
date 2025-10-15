#!/usr/bin/env bash

set -euo pipefail

FILES=("$@")

if [[ ${#FILES[@]} -eq 0 ]]
then
  mapfile -t FILES < <(git ls-files '*CHANGELOG.md')
fi

clrt changelogs -i "${FILES[@]}"

if ! git diff -s --exit-code "${FILES[@]}"
then
  if [ -t 2 ]
  then
    # shellcheck disable=SC2016
    echo 'Some changelogs were modified - use `git diff` to see the changes' >&2
  fi
  false
fi
