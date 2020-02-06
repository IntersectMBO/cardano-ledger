#!/usr/bin/env bash
nix build -f `dirname $0`/.. haskellPackages.cardano-ledger.components.tests.cardano-ledger-test -o cardano-ledger-test
cd cardano-ledger && ../cardano-ledger-test/bin/cardano-ledger-test --scenario=QualityAssurance
