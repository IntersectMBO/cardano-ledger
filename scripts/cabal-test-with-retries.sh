#!/usr/bin/env bash

set -euo pipefail

PACKAGE=$1
TRIES=$2
CONDITION=${3-'Gave up! Passed only [0-9]* tests; [0-9]* discarded tests'}

for TRY in $(seq "$TRIES"); do
  echo ">>>>> Testing $PACKAGE ... attempt $TRY of $TRIES <<<<<"
  if cabal test "$PACKAGE"; then
    exit 0
  elif ! find dist-newstyle -path '*/t/*' -name "$PACKAGE*.log" | xargs grep -h "$CONDITION"; then
    echo "The test failure isn't retryable - aborting"
    exit 1
  fi
done

echo "Tests failed $TRY times - aborting"
exit 2
