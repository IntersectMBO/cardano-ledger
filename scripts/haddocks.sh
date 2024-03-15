#!/usr/bin/env bash

# Build haddock documentation and an index for all projects in
# `cardano-ledger` repository.
#
# usage:
# ./haddocks.sh directory [components ...]
#
# $1 - where to put the generated pages, this directory contents will be wiped
#      out (so don't pass `/` or `./` - the latter will delete your 'dist-newstyle')
#      (the default is 'haddocks')
# $2 - the components to re-build haddocks for, or 'all'
#      (the default is none)
#

set -euo pipefail

OUTPUT_DIR=${1:-haddocks}
REGENERATE=("${@:2}")

BUILD_DIR=dist-newstyle

CABAL_OPTS=(
  --builddir "${BUILD_DIR}"
  --enable-benchmarks
  --enable-documentation
  --enable-tests
)

# Generate  `doc-index.json` and `doc-index.html` per package, to assemble them later at the top level.
HADDOCK_OPTS=(
  --haddock-all
  --haddock-html
  --haddock-hyperlink-source
  --haddock-internal
  --haddock-option "--show-all"
  --haddock-option "--use-unicode"
  --haddock-option="--base-url=.."
  --haddock-quickjump
)

# Rebuild documentation if requested
if (( "${#REGENERATE[@]}" > 0 )); then
  cabal build   "${CABAL_OPTS[@]}" "${REGENERATE[@]}"
  cabal haddock "${CABAL_OPTS[@]}" "${REGENERATE[@]}" "${HADDOCK_OPTS[@]}"
fi

if [[ ! -d "${OUTPUT_DIR}" ]]; then
  mkdir -p "${OUTPUT_DIR}"
fi

# make all files user writable
chmod -R u+w "${OUTPUT_DIR}"

GHC_VERSION=$(ghc --numeric-version)
OS_ARCH=$(jq -r '"\(.arch)-\(.os)"' "$BUILD_DIR/cache/plan.json")

# copy the new docs
for package_dir in "${BUILD_DIR}/build/${OS_ARCH}/ghc-${GHC_VERSION}"/*; do
  package=$(basename "${package_dir}" | sed 's/-[0-9]\+\(\.[0-9]\+\)*//')
  if [ -d "${package_dir}/doc/html/${package}" ]; then
    cp -r "${package_dir}/doc/html/${package}" "${OUTPUT_DIR}"
  else continue
  fi
  # copy test packages documentation when it exists
  if [ -d "${package_dir}/t" ]; then
    for test_package_dir in "${package_dir}/t"/*; do
      test_package=$(basename "${test_package_dir}")
      if [ -d "${test_package_dir}/doc/html/${package}/${test_package}" ]; then
        cp -r "${test_package_dir}/doc/html/${package}/${test_package}" "${OUTPUT_DIR}/${package}:${test_package}"
        cp -n "${package_dir}/doc/html/${package}"/{*.css,*.js}  "${OUTPUT_DIR}/${package}:${test_package}"
      fi
    done
  fi
  # copy lib packages documentation when it exists
  if [ -d "${package_dir}/l" ]; then
    for lib_package_dir in "${package_dir}/l"/*; do
      lib_package=$(basename "${lib_package_dir}")
      if [ -d "${lib_package_dir}/doc/html/${package}" ]; then
        cp -r "${lib_package_dir}/doc/html/${package}" "${OUTPUT_DIR}/${package}:${lib_package}"
        cp -n "${package_dir}/doc/html/${package}"/{*.css,*.js}  "${OUTPUT_DIR}/${package}:${lib_package}"
      fi
    done
  fi
done

# build read-interface arguments for haddock
interface_options=()
for package_dir in "${OUTPUT_DIR}"/*; do
  package=$(basename "${package_dir}")
  if [[ -d "${package_dir}" ]]; then
    haddock_files=("${package_dir}"/*.haddock)
    interface_options+=("--read-interface=${package},${haddock_files[0]}")
  fi
done

./scripts/mkprolog.sh "${OUTPUT_DIR}" scripts/prolog

# Generate top level index using interface files
haddock \
  -o "${OUTPUT_DIR}" \
  --title "cardano-ledger" \
  --package-name "Cardano Ledger" \
  --gen-index \
  --gen-contents \
  --quickjump \
  --prolog ./scripts/prolog \
  "${interface_options[@]}"

# Assemble a toplevel `doc-index.json` from package level ones.
for file in "$OUTPUT_DIR"/*/doc-index.json; do
  project=$(basename "$(dirname "$file")");
  jq ".[] | .link = \"${project}/\(.link)\"" "${file}"
done |
  jq -s . >"${OUTPUT_DIR}/doc-index.json"
