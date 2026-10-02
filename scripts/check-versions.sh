#!/usr/bin/env bash

# Check that the version in each package's cabal file agrees with its `CHANGELOG.md`.
#
# As per `RELEASING.md`, a developer introducing a change must bump the version in both
# the `CHANGELOG.md` and the `.cabal` file in the same PR.
#
# The `CHANGELOG` is the authority on the intended version. The developer chooses whether
# a change deserves a patch, minor or major bump and records that as the top section,
# while `bump-changelogs.sh` only ever sets the floor. The cabal version therefore
# follows the `CHANGELOG` and must never run ahead of it.
#
# The two are meant to disagree in exactly one state, the one that `bump-changelogs.sh`
# creates right after a release: an empty placeholder section (a lone `*`) on top, with
# the cabal version left behind on the released version. That lag is not slack to be
# taken up, it is required. The placeholder only reserves the next version number, it
# does not claim that the version is worth releasing, and `RELEASING.md` has the release
# engineer release every package whose cabal version is ahead of the one on CHaP. Moving
# the cabal version onto an empty section therefore queues up a release with nothing to
# show for it. Whatever makes a version releasable, a dependency bounds bump included,
# earns an entry in the `CHANGELOG`.
#
# The lag can span more than one section when several releases happened without any
# change in between.
#
# Five invariants are checked, in this order so that the most specific diagnostic wins:
#
#   1. The top section holds the highest version of all the sections within that same
#      `CHANGELOG`, which is to say the sections are in descending order. The cabal
#      version plays no part in this one.
#   2. The cabal version does not exceed the top section's version.
#   3. The cabal version appears as some section in the `CHANGELOG`.
#   4. If the top section has entries, the cabal version matches it.
#   5. If the top section has no entries, the cabal version stays below it.
#
# The magnitude of the bump is not checked, only that the two files agree.
#
# Note that packages excluded from the release process are neither versioned nor have a
# `CHANGELOG`, so skipping those without one is enough to leave them alone.

set -euo pipefail

EXIT_CODE=0

# Highest of the versions given as arguments, compared as numbers and not lexicographically.
#
#   $ highest_version 1.2.3.0 1.10.0.0 1.2.3.1
#   1.10.0.0
#
# A lexicographic sort would pick `1.2.3.1` here, because it compares the `10` in `1.10`
# one character at a time.
highest_version() {
  printf '%s\n' "$@" | LANG=C sort -V | tail -1
}

for CABAL_LOCAL_FILE_PATH in $(git ls-files '*.cabal');
do
  # Extract the name of the package (without the path and the extension)
  PACKAGE_NAME=$(basename "$CABAL_LOCAL_FILE_PATH" .cabal)
  # Construct the path to the package's `CHANGELOG`
  CHANGELOG_PATH=${CABAL_LOCAL_FILE_PATH/$PACKAGE_NAME.cabal/CHANGELOG.md}

  # Skip packages that are excluded from the release process, which is to say those
  # without a `CHANGELOG`: they are not versioned, so there is nothing to compare
  [[ -f "$CHANGELOG_PATH" ]] || continue

  CABAL_VERSION=$(awk 'tolower($1) == "version:" { print $2; exit }' "$CABAL_LOCAL_FILE_PATH")

  # `version:` is mandatory in a cabal file, so an empty result means the
  # extraction above missed it - a layout that `cabal` accepts but the pattern
  # does not. Report that rather than silently passing the package.
  if [[ -z "$CABAL_VERSION" ]]; then
    printf "%s: no 'version:' field found\n" "$CABAL_LOCAL_FILE_PATH"
    EXIT_CODE=1
    continue
  fi

  # Every version that has a section in the `CHANGELOG`, in the order they appear
  mapfile -t CHANGELOG_VERSIONS < <(awk '/^## +[0-9]/ { print $2 }' "$CHANGELOG_PATH")

  # A `CHANGELOG` without a single `## <version>` heading leaves nothing to compare the
  # cabal version against, so it is a malformed file rather than a package to skip. The
  # guard is also what makes the indexing below safe.
  if [[ ${#CHANGELOG_VERSIONS[@]} -eq 0 ]]; then
    printf "%s: no versioned section found\n" "$CHANGELOG_PATH"
    EXIT_CODE=1
    continue
  fi
  # The most recent section, which every invariant below takes as the intended version
  TOP_CHANGELOG_VERSION=${CHANGELOG_VERSIONS[0]}

  # Invariant 1
  if [[ "$TOP_CHANGELOG_VERSION" != "$(highest_version "${CHANGELOG_VERSIONS[@]}")" ]]; then
    printf "%s: top section is %s, but a later section has a higher version\n" \
      "$CHANGELOG_PATH" "$TOP_CHANGELOG_VERSION"
    EXIT_CODE=1
    continue
  fi

  # Invariant 2. The `CHANGELOG` decides the intended version, so a cabal
  # version that is ahead of it is always wrong.
  if [[ "$CABAL_VERSION" != "$TOP_CHANGELOG_VERSION" &&
        "$CABAL_VERSION" == "$(highest_version "$CABAL_VERSION" "$TOP_CHANGELOG_VERSION")" ]]; then
    printf "%s: version is %s, which is ahead of %s whose most recent section is %s - set the cabal version to %s, or add the missing section\n" \
      "$CABAL_LOCAL_FILE_PATH" "$CABAL_VERSION" "$CHANGELOG_PATH" "$TOP_CHANGELOG_VERSION" "$TOP_CHANGELOG_VERSION"
    EXIT_CODE=1
    continue
  fi

  # Invariant 3
  FOUND=no
  for VERSION in "${CHANGELOG_VERSIONS[@]}";
  do
    if [[ "$VERSION" == "$CABAL_VERSION" ]]; then
      FOUND=yes
      break
    fi
  done
  if [[ "$FOUND" == no ]]; then
    printf "%s: version is %s, but %s has no section for it\n" \
      "$CABAL_LOCAL_FILE_PATH" "$CABAL_VERSION" "$CHANGELOG_PATH"
    EXIT_CODE=1
    continue
  fi

  # Whether the top section holds any entries, as opposed to the lone `*` placeholder
  TOP_CHANGELOG_VERSION_HAS_ENTRIES=$(awk '
    /^## +[0-9]/ { if (++section > 1) exit; next }
    section == 1 {
      line = $0
      gsub(/^[[:space:]]+|[[:space:]]+$/, "", line)
      if (line != "") { total += 1; if (line == "*") stars += 1 }
    }
    END { print (total == 0 || (total == 1 && stars == 1)) ? "no" : "yes" }
  ' "$CHANGELOG_PATH")

  # Invariant 4. The cabal version is known to be at or below the top section by now, so
  # a mismatch here always means the cabal file is the one lagging behind.
  #
  # Entries under the top section mean the developer settled on that version, so the
  # cabal file has to catch up. With a cabal version of `1.2.0.0`:
  #
  #   ## 1.3.0.0      <- an error, the cabal version must be bumped to 1.3.0.0
  #   * Add `foo`
  #
  # A lone `*` is instead the placeholder that `bump-changelogs.sh` leaves behind after a
  # release, and the cabal version is supposed to stay on the released version until
  # something is actually added. Again with a cabal version of `1.2.0.0`:
  #
  #   ## 1.3.0.0      <- fine, nothing has been added since 1.2.0.0 was released
  #   *
  #
  #   ## 1.2.0.0
  #   * Add `foo`
  if [[ "$TOP_CHANGELOG_VERSION_HAS_ENTRIES" == yes && "$CABAL_VERSION" != "$TOP_CHANGELOG_VERSION" ]]; then
    printf "%s: version is %s, but %s has entries under %s - bump the cabal version to %s\n" \
      "$CABAL_LOCAL_FILE_PATH" "$CABAL_VERSION" "$CHANGELOG_PATH" "$TOP_CHANGELOG_VERSION" "$TOP_CHANGELOG_VERSION"
    EXIT_CODE=1
  fi

  # Invariant 5. The mirror of invariant 4. An empty top section only reserves the next
  # version number, so the cabal version has to stay on the released version below it -
  # otherwise the release engineer ships a version whose section records no change.
  #
  # The error is invariant 4's example read the other way round. With a cabal version of
  # `1.3.0.0`:
  #
  #   ## 1.3.0.0      <- an error, nothing is recorded under 1.3.0.0
  #   *
  #
  #   ## 1.2.0.0
  #   * Add `foo`
  #
  # The fix is whichever of the two matches what actually happened: leave the cabal
  # version on `1.2.0.0` if nothing has changed since it was released, or record the
  # change under `1.3.0.0` if something has. A dependency bounds bump is the common way
  # to land here and takes the second route - it is a real reason to release the package,
  # so it earns a real entry.
  if [[ "$TOP_CHANGELOG_VERSION_HAS_ENTRIES" == no &&
        "$CABAL_VERSION" == "$TOP_CHANGELOG_VERSION" ]]; then
    printf "%s: version is %s, but %s has no entries under %s - either record the change under %s, or leave the cabal version on the version that was released\n" \
      "$CABAL_LOCAL_FILE_PATH" "$CABAL_VERSION" "$CHANGELOG_PATH" "$TOP_CHANGELOG_VERSION" "$TOP_CHANGELOG_VERSION"
    EXIT_CODE=1
  fi
done

if [[ "$EXIT_CODE" -ne 0 ]]; then
  printf "\n%s\n%s\n" \
    "See RELEASING.md: the version must be updated in both the CHANGELOG.md and the" \
    ".cabal file of every package affected by a change, in the same PR."
fi

exit "$EXIT_CODE"
