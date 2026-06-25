#!/usr/bin/env bash

set -euo pipefail

type shellcheck

git ls-files scripts | xargs -rd\\n file | grep -E 'Bourne|bash' | cut -d: -f1 | xargs -rtd\\n shellcheck
