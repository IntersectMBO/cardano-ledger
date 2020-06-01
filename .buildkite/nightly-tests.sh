#!/usr/bin/env bash
nix build -f `dirname $0`/.. haskellPackages.shelley-spec-ledger.components.tests.shelley-spec-ledger-test -o shelley-spec-ledger-test
pushd shelley/chain-and-ledger/executable-spec/ \
  && nix-shell ../../../shell.nix --run \
  "../../../shelley-spec-ledger-test/bin/shelley-spec-ledger-test --scenario=Nightly"

popd
nix build -f `dirname $0`/.. haskellPackages.cardano-ledger.components.tests.cardano-ledger-test -o cardano-ledger-test
pushd byron/ledger/impl \
  && ../../../cardano-ledger-test/bin/cardano-ledger-test --scenario=QualityAssurance

popd
