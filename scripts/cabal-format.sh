#!/usr/bin/env bash

set -euo pipefail

mode="${1:-format}"

if [[ "$mode" != "format" && "$mode" != "check" ]]; then
  echo "Error: Invalid mode: ${mode}. Allowed values are 'format' or 'check'." >&2
  exit 1
fi

git ls-files -- '*.cabal' 'cabal.project' | while IFS= read -r f; do
  cmd=(cabal-gild -i "$f" -m "$mode")

  if [[ "$mode" == "format" ]]; then
    cmd+=(-o "$f")
  fi

  "${cmd[@]}"
done

if [[ "$mode" == "format" ]]; then
  git diff --exit-code
fi
