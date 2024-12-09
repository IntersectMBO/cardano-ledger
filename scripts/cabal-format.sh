#!/usr/bin/env bash

set -euo pipefail

if [[ "${1:-format}" != "format" && "${1:-format}" != "check" ]]; then
  echo "Error: Invalid mode. Allowed values are 'format' or 'check'." >&2
  exit 1
fi

mode="${1:-format}"

git ls-files -- '*.cabal' | while IFS= read -r f; do
  cmd=(cabal-gild -i "$f" -m "$mode")

  if [[ "$mode" == "format" ]]; then
    cmd+=(-o "$f")
  fi

  printf '%s ' "${cmd[@]}"
  printf '\n'
  "${cmd[@]}"
done

if [[ "$mode" == "format" ]]; then
  git diff --exit-code
fi
