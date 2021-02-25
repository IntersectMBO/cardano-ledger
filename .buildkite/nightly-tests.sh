#!/usr/bin/env bash
set -e

nix build -f `dirname $0`/.. haskellPackages.cardano-ledger-shelley-ma-test.components.tests.cardano-ledger-shelley-ma-test -o cardano-ledger-shelley-ma-test
pushd shelley-ma/shelley-ma-test/
nix-shell ../../shell.nix --run \
  "../../cardano-ledger-shelley-ma-test/bin/cardano-ledger-shelley-ma-test --scenario=Nightly"
popd

nix build -f `dirname $0`/.. haskellPackages.shelley-spec-ledger-test.components.tests.shelley-spec-ledger-test -o shelley-spec-ledger-test
pushd shelley/chain-and-ledger/shelley-spec-ledger-test/
nix-shell ../../../shell.nix --run \
  "../../../shelley-spec-ledger-test/bin/shelley-spec-ledger-test --scenario=Nightly"
popd

nix build -f `dirname $0`/.. haskellPackages.cardano-ledger-byron.components.tests.cardano-ledger-byron-test -o cardano-ledger-byron-test
pushd byron/ledger/impl
../../../cardano-ledger-byron-test/bin/cardano-ledger-byron-test --scenario=QualityAssurance
popd
