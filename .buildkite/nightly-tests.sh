#!/usr/bin/env bash
nix build -f `dirname $0`/.. haskellPackages.shelley-spec-ledger.components.tests.shelley-spec-ledger-test -o shelley-specs-ledger-tests
# cd cardano-ledger && ../cardano-ledger-test/bin/cardano-ledger-test --scenario=QualityAssurance
