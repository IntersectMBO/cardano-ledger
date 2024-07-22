#!/usr/bin/env bash

set -euo pipefail

if [ ! -f "$1" ]; then
  echo "Error: $1 does not exist!"
  exit 1
fi

if [ ! -s "$1" ]; then
  echo "Error: $1 is empty!"
  exit 1
fi

echo "$1 exists and is non-empty. Printing..."
cat "$1"