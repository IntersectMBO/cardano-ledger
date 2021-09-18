#!/usr/bin/env bash
set -e

nix build -f `dirname $0`/.. haskellPackages.cardano-ledger-alonzo-test.components.tests.cardano-ledger-alonzo-test -o cardano-ledger-alonzo-test
pushd eras/alonzo/test-suite/
nix-shell ../../shell.nix --run \
  "../../cardano-ledger-alonzo-test/bin/cardano-ledger-alonzo-test --scenario=Nightly"
popd

nix build -f `dirname $0`/.. haskellPackages.cardano-ledger-shelley-ma-test.components.tests.cardano-ledger-shelley-ma-test -o cardano-ledger-shelley-ma-test
pushd eras/shelley-ma/test-suite/
nix-shell ../../shell.nix --run \
  "../../cardano-ledger-shelley-ma-test/bin/cardano-ledger-shelley-ma-test --scenario=Nightly"
popd

nix build -f `dirname $0`/.. haskellPackages.cardano-ledger-shelley-test.components.tests.cardano-ledger-shelley-test -o cardano-ledger-shelley-test
pushd eras/shelley/test-suite/
nix-shell ../../../shell.nix --run \
  "../../../cardano-ledger-shelley-test/bin/cardano-ledger-shelley-test --scenario=Nightly"
popd

#nix build -f `dirname $0`/.. haskellPackages.cardano-ledger-byron.components.tests.cardano-ledger-byron-test -o cardano-ledger-byron-test
#pushd byron/ledger/impl
#../../../cardano-ledger-byron-test/bin/cardano-ledger-byron-test --scenario=QualityAssurance
#popd
#
# Two bryon property tests are currently failing:
#   1 ts_prop_mainnetEpochsValid
#   2 prop_deserializeEpochs
# See:
#   1 --hedgehog-replay "Size 0 Seed 18072700249420076377 13145888230004625323"
#   2 --hedgehog-replay "Size 0 Seed 9707308386760880353 4199123723211136257"
