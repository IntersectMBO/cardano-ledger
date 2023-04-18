#!/usr/bin/env bash

# Build haddock documentation and an index for all projects in
# `cardano-ledger` repository.
#
# usage:
# ./haddocks.sh directory [true|false]
#
# $1 - where to put the generated pages, this directory contents will be wiped
#      out (so don't pass `/` or `./` - the latter will delete your 'dist-newstyle')
#      (the default is './haddocks')
# $2 - whether to re-build haddocks with `cabal haddock` command or a component name
#      (the default is true)
#

set -euo pipefail

OUTPUT_DIR=${1:-"./haddocks"}
REGENERATE=${2:-"true"}

BUILD_DIR="dist-newstyle"
GHC_VERSION=$(ghc --numeric-version)
OS_ARCH="$(cat dist-newstyle/cache/plan.json | jq -r '.arch + "-" + .os' | head -n 1 | xargs)"


# Generate  `doc-index.json` and `doc-index.html` per package, to assemble them later at the top level.
HADDOCK_OPTS=(
    --builddir "${BUILD_DIR}"
    --haddock-all
    --haddock-internal
    --haddock-html
    --haddock-quickjump
    --haddock-hyperlink-source
    --haddock-option "--show-all"
    --haddock-option "--use-unicode"
    --haddock-option="--base-url=.."
  )

# build documentation of all modules
if [ ${REGENERATE} == "true" ]; then
  cabal haddock "${HADDOCK_OPTS[@]}" all
elif [ ${REGENERATE} != "false" ]; then
  cabal haddock "${HADDOCK_OPTS[@]}" ${REGENERATE}
fi

if [[ !( -d ${OUTPUT_DIR} ) ]]; then
  mkdir -p ${OUTPUT_DIR}
fi

# make all files user writable
chmod -R u+w "${OUTPUT_DIR}"

# copy the new docs
for dir in $(ls "${BUILD_DIR}/build/${OS_ARCH}/ghc-${GHC_VERSION}"); do
  package=$(echo "${dir}" | sed 's/-[0-9]\+\(\.[0-9]\+\)*//')
  if [ -d "${BUILD_DIR}/build/${OS_ARCH}/ghc-${GHC_VERSION}/${dir}/doc/html/${package}" ]; then
    cp -r "${BUILD_DIR}/build/${OS_ARCH}/ghc-${GHC_VERSION}/${dir}/doc/html/${package}" ${OUTPUT_DIR}
  else continue;
  fi
  # copy test packages documentation when it exists
  if [ -d "${BUILD_DIR}/build/${OS_ARCH}/ghc-${GHC_VERSION}/${dir}/t" ]; then
      for test_package in $(ls "${BUILD_DIR}/build/${OS_ARCH}/ghc-${GHC_VERSION}/${dir}/t"); do
          if [ -d "${BUILD_DIR}/build/${OS_ARCH}/ghc-${GHC_VERSION}/${dir}/t/${test_package}/doc/html/${package}/${test_package}" ]; then
              cp -r "${BUILD_DIR}/build/${OS_ARCH}/ghc-${GHC_VERSION}/${dir}/t/${test_package}/doc/html/${package}/${test_package}" "${OUTPUT_DIR}/${package}:${test_package}"
              cp -n "${BUILD_DIR}/build/${OS_ARCH}/ghc-${GHC_VERSION}/${dir}/doc/html/${package}"/{*.css,*.js}  "${OUTPUT_DIR}/${package}:${test_package}"
          fi
      done
  fi
  # copy lib packages documentation when it exists
  if [ -d "${BUILD_DIR}/build/${OS_ARCH}/ghc-${GHC_VERSION}/${dir}/l" ]; then
      for lib_package in $(ls "${BUILD_DIR}/build/${OS_ARCH}/ghc-${GHC_VERSION}/${dir}/l"); do
          if [ -d "${BUILD_DIR}/build/${OS_ARCH}/ghc-${GHC_VERSION}/${dir}/l/${lib_package}/doc/html/${package}" ]; then
              cp -r "${BUILD_DIR}/build/${OS_ARCH}/ghc-${GHC_VERSION}/${dir}/l/${lib_package}/doc/html/${package}" "${OUTPUT_DIR}/${package}:${lib_package}"
              cp -n "${BUILD_DIR}/build/${OS_ARCH}/ghc-${GHC_VERSION}/${dir}/doc/html/${package}"/{*.css,*.js}  "${OUTPUT_DIR}/${package}:${lib_package}"
          fi
      done
  fi
done

# build read-interface arguments for haddock
interface_options () {
    for package in $(ls "${OUTPUT_DIR}"); do
        if [[ -d "${OUTPUT_DIR}/${package}" ]]; then
            haddock_file=$(ls -1 ${OUTPUT_DIR}/${package}/*.haddock | head -1)
            echo "--read-interface=${package},${haddock_file}"
        fi
  done
}

./scripts/mkprolog.sh ./haddocks ./scripts/prolog

# Generate top level index using interface files
haddock \
  -o ${OUTPUT_DIR} \
  --title "cardano-ledger" \
  --package-name "Cardano Ledger" \
  --gen-index \
  --gen-contents \
  --quickjump \
  --prolog ./scripts/prolog \
  $(interface_options)

# Assemble a toplevel `doc-index.json` from package level ones.
echo "[]" > "${OUTPUT_DIR}/doc-index.json"
for file in $(ls $OUTPUT_DIR/*/doc-index.json); do
  project=$(basename $(dirname $file));
  jq -s \
    ".[0] + [.[1][] | (. + {link: (\"${project}/\" + .link)}) ]" \
    "${OUTPUT_DIR}/doc-index.json" \
    ${file} \
    > /tmp/doc-index.json
  mv /tmp/doc-index.json "${OUTPUT_DIR}/doc-index.json"
done
