#!/usr/bin/env bash
nix build -f `dirname $0`/.. haskellPackages.shelley-spec-ledger.components.tests.shelley-spec-ledger-test -o shelley-spec-ledger-test
cd shelley/chain-and-ledger/executable-spec/ \
  && nix-shell ../../../shell.nix --run \
  "../../../shelley-spec-ledger-test/bin/shelley-spec-ledger-test --scenario=Nightly"
