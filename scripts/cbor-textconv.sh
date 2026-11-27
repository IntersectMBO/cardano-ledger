#!/usr/bin/env bash

set -euo pipefail

exec 2>&1 # Include stderr in the diff output, in case this is run from a GUI program

# `fourmolu` can be very slow on large single-line expressions, so we don't try to prettify
# files above a certain size.
#
# The default size limit is 256K. A different limit can be specified using the git configuration
# variable `diff.cbor.maxBytes`. Use a negative value to disable limiting, and `0` to apply
# limiting to all files (thus disabling pretty-printed diffing completely).
# Use `git -c diff.cbor.maxBytes=VALUE ...` to override the setting temporarily.
# Git supports unit suffixes like k/K and m/M *when reading from config files*.
MAX_SIZE=$(git config get --type=int diff.cbor.maxBytes 2>/dev/null) || MAX_SIZE=$((256 * 1024))

FILE=$1
SIZE=$(wc -c <"$FILE")
SIZE=${SIZE//[[:space:]]/} # Some `wc` implementations pad the number

FAILURE=

if [[ $MAX_SIZE -ge 0 ]] && [[ $SIZE -ge $MAX_SIZE ]]
then
  FAILURE='file too large to pretty-print'
elif ! cuddle format-cbor --fin binary --fout term "$FILE" | fourmolu -q --no-cabal
then
  FAILURE='pretty-printing failed'
fi

if [[ $FAILURE ]]
then
  echo "# CBOR $FAILURE; length $SIZE; digest $(sha256sum "$1" | cut -d' ' -f1)"
fi
