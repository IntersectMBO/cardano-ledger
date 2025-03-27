#!/usr/bin/env bash

CHAP=./cardano-haskell-packages
# Download a shallow copy of CHaP
git clone --depth 1 git@github.com:IntersectMBO/cardano-haskell-packages.git $CHAP

cd $CHAP || exit
# Save all the available packages from CHaP
CHAP_PACKAGES=$(./scripts/list-packages.sh)
cd - || exit
echo "The following packages are available in CHaP:"
echo "$CHAP_PACKAGES"

# Save the paths to every `cardano-ledger` cabal file
CABAL_FILES=$(find . -wholename '*/eras/*.cabal' -o -wholename '*/libs/*.cabal')

for i in $CABAL_FILES;
do
  # Extract the name of the package (without the path and the extension)
  PACKAGE=$(basename "$i" .cabal)
  # Construct the path to the package's `CHANGELOG`
  CHANGELOG=${i/$PACKAGE.cabal/CHANGELOG.md}

  # Check if package has a `CHANGELOG`
  if [[ -f "$CHANGELOG" ]]; then
    # Get the most recent version number in the `CHANGELOG`
    VERSION=$(grep -m 1 -o "[0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+" "$CHANGELOG")

    # Check if the package had a release with
    # the most recent `CHANGELOG` version number
    printf "Looking for %s with version %s in CHaP\n" "$PACKAGE" "$VERSION"
    RESULT=$(echo "$CHAP_PACKAGES" | grep -o "$PACKAGE $VERSION")
    if [[ -n "$RESULT" ]]; then
      # A release was found and thus the `CHANGELOG` has to be bumped
      # with the incremented patch version
      NEXT_VERSION=$(echo "$VERSION" | awk -F. -v OFS=. '{$NF += 1 ; print}')
      printf "Bumping %s to %s\n" "$CHANGELOG" "$NEXT_VERSION"
      sed -i "s/## $VERSION/## $NEXT_VERSION\n\n*\n\n## $VERSION/" "$CHANGELOG"
    fi
  fi
done

rm -rf $CHAP
