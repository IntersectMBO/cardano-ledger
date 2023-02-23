#!/usr/bin/env bash

set -euo pipefail

gen-hie > hie.yaml

git diff --exit-code