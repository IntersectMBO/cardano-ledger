#!/usr/bin/env bash
nix build -f `dirname $0`/../default.nix nix-tools.tests.cardano-ledger.cardano-ledger-test -o cardano-ledger-test
cd cardano-ledger && ../cardano-ledger-test/bin/cardano-ledger-test --scenario=QualityAssurance
