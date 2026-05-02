#!/usr/bin/env bash

set -euo pipefail

# Packages to run doctests for; defaults to all packages if none are specified
PACKAGES=("$@")

# Install doctest, if it's not present already
if [[ -z "$(type -t doctest)" ]]
then
  cabal install doctest --ignore-project --overwrite-policy=always
  PATH=$(cabal path --installdir):$PATH
fi

# Ensure doctest and PATH are using the same ghc version

getExecutablePath()
{
  "$1" -package-env - -e 'import System.Environment' -e 'putStrLn =<< getExecutablePath'
}

lookupVar()
{
  ghc -e 'interact $ maybe "" id . lookup "'"$1"'" . read'
}

getPackageDb()
{
  readlink -f "$("$1" --info | lookupVar 'Global Package DB')"
}

default_ghc=$(type -p ghc)
doctest_ghc=$(doctest --info | lookupVar 'ghc')

if [[ "$(getExecutablePath "$default_ghc")" != "$(getExecutablePath "$doctest_ghc")" ]]
then
  echo "Incompatible GHC's:" >&2
  echo "  Default ghc: $(getExecutablePath "$default_ghc")" >&2
  echo "  Doctest ghc: $(getExecutablePath "$doctest_ghc")" >&2
  exit 1
fi

if [[ "$(getPackageDb ghc)" != "$(getPackageDb doctest)" ]]
then
  echo "Incompatible package DBs:" >&2
  echo "  Default DB: $(getPackageDb ghc)" >&2
  echo "  Doctest DB: $(getPackageDb doctest)" >&2
  exit 1
fi

# Specify additional arguments needed for specific modules
EXTRA_ARGS=$(mktemp)
trap 'rm -f "$EXTRA_ARGS"' EXIT
cat <<EOF >"$EXTRA_ARGS"
cardano-ledger-api:lib:cardano-ledger-api --build-depends=cardano-ledger-babbage:testlib
EOF

RESULT=0
shopt -s lastpipe # Run the while loop in the current process, to enable setting RESULT=1

# Run the doctests for some or all packages
cleret cabal targets "${PACKAGES[@]}" |
sort | join -t' ' -a1 -j1 - "$EXTRA_ARGS" |
while read -ra ARGS
do
  echo "***** cabal doctest ${ARGS[0]} *****"
  cabal repl --with-repl=doctest --repl-options='-w -Wdefault' \
    --build-depends=QuickCheck --build-depends=template-haskell \
      "${ARGS[@]}" || RESULT=1
done

exit "$RESULT"
