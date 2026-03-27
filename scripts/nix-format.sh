#!/usr/bin/env bash

set -euo pipefail

type nixfmt

git ls-files -- '*.nix' | xargs -r nixfmt

# If any file changed, this line returns exit code 1 (CI fails)
# If nothing changed, exit code 0 is returned (CI passes)
git diff --exit-code
