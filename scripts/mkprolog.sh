#!/usr/bin/env bash

set -euo pipefail

HADDOCKS_DIR=${1:-"./haddocks"}
PROLOG_FILE=${2:-"./scripts/prolog"}

> ${PROLOG_FILE}

cat > ${PROLOG_FILE} << EOF
= Cardano Ledger Repository Hackage Documentation

[skip to module list](#module-list)

This site contains Haskell documentation of:

EOF

for dir in $(ls ${HADDOCKS_DIR}); do
  if [[ -d ${HADDOCKS_DIR}/${dir} ]]; then
    link=$(echo "${dir}" | sed "s/:/%3A/g")
    echo "* __[${dir}](${link}/index.html)__" >> ${PROLOG_FILE}
  fi
done
