#!/usr/bin/env nix-shell
#!nix-shell -i bash -p git bash nixStable

# check and warn if `pkgs/default.nix` is out of date

set -euo pipefail

fail_nix_tools_check() {
  # The '-w' option for 'git diff' is used because in some builds
  # (i.e. https://buildkite.com/input-output-hk/cardano-sl/builds/976#0fb162df-8f9b-42d7-9ca7-608a9ea06d4d)
  # the patch to 'default.nix' only suggests that whitespaces be added
  # (see https://gist.github.com/anonymous/f52dbb040db16034d303e27056a0a48e), without
  # which the build fails in the 'stack2nix' step.
  git diff -w --text > /tmp/nix-tools.patch
  buildkite-agent artifact upload /tmp/nix-tools.patch --job "$BUILDKITE_JOB_ID"
  echo "ERROR: you need to (run ./scripts/nix-tools-generate.sh or apply the patch in the buildkite artifact) and commit the changes" >&2
  exit 1
}

THIS="${BASH_SOURCE[0]%/*}"
exec "${THIS}/../nix-tools-generate.sh"

git diff -w --text --exit-code || fail_nix_tools_check
