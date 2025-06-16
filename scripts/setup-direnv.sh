#!/usr/bin/env bash

######
# Utility script for setting up `direnv`.
# The script is extremely minimal and assumes you have `direnv` installed.

echo "use flake .#pre-commit" > .envrc
direnv allow
