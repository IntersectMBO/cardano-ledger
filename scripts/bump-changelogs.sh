#!/usr/bin/env bash

# Save all the available packages from CHaP
CHAP_PACKAGES=$(mktemp)
trap 'rm -f "$CHAP_PACKAGES"' EXIT

if tar --version | grep -q 'GNU tar'
then
  tar () { command tar --wildcards "$@"; }
fi

curl -sSL https://chap.intersectmbo.org/01-index.tar.gz |
  tar -tz \*.cabal |
  cut -d/ -f1-2 |
  LANG=C sort -t/ -k1,1 -k2,2Vr |
  LANG=C sort -t/ -k1,1 -u -o "$CHAP_PACKAGES"
echo "The complete CHaP package index is here:"
echo "$CHAP_PACKAGES"

# Save the paths to every `cardano-ledger` cabal file
CABAL_FILES=$(git ls-files '*.cabal')

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
    RESULT=$(grep -o "$PACKAGE/$VERSION" "$CHAP_PACKAGES")
    if [[ -n "$RESULT" ]]; then
      # A release was found and thus the `CHANGELOG` has to be bumped
      # with the incremented patch version
      NEXT_VERSION=$(echo "$VERSION" | awk -F. -v OFS=. '{$NF += 1 ; print}')
      printf "Bumping %s to %s\n" "$CHANGELOG" "$NEXT_VERSION"
      sed -i "s/## $VERSION/## $NEXT_VERSION\n\n*\n\n## $VERSION/" "$CHANGELOG"
    fi
  fi
done

if ! git diff -s --exit-code; then
  printf "\n!!!!!!\n%s %s\n!!!!!!\n" \
    "WARNING! DO NOT BUMP THE VERSION NUMBER IN THE CABAL FILES" \
    "(unless their dependencies were bumped)!"
fi
