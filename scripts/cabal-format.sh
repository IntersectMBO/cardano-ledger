#!/usr/bin/env bash

set -euo pipefail

# if [[ "${1:-format}" != "format" && "${1:-format}" != "check" ]]; then
#   echo "Error: Invalid mode. Allowed values are 'format' or 'check'." >&2
#   exit 1
# fi

# mode="${1:-format}"

# git ls-files -- '*.cabal' | while IFS= read -r f; do
#   cmd=(cabal-gild -i "$f" -m "$mode")

#   if [[ "$mode" == "format" ]]; then
#     cmd+=(-o "$f")
#   fi

#   # "${cmd[@]}"
#   printf '%s ' "${cmd[@]}"
#   printf '\n'
# done

# if [[ "$mode" == "format" ]]; then
#   git diff --exit-code
# fi

cabal-gild -i eras/allegra/impl/cardano-ledger-allegra.cabal -m check
cabal-gild -i eras/alonzo/impl/cardano-ledger-alonzo.cabal -m check
cabal-gild -i eras/alonzo/test-suite/cardano-ledger-alonzo-test.cabal -m check
cabal-gild -i eras/babbage/impl/cardano-ledger-babbage.cabal -m check
cabal-gild -i eras/babbage/test-suite/cardano-ledger-babbage-test.cabal -m check
cabal-gild -i eras/byron/chain/executable-spec/byron-spec-chain.cabal -m check
cabal-gild -i eras/byron/crypto/cardano-crypto-wrapper.cabal -m check
cabal-gild -i eras/byron/crypto/test/cardano-crypto-test.cabal -m check
cabal-gild -i eras/byron/ledger/executable-spec/byron-spec-ledger.cabal -m check
cabal-gild -i eras/byron/ledger/impl/cardano-ledger-byron.cabal -m check
cabal-gild -i eras/byron/ledger/impl/test/cardano-ledger-byron-test.cabal -m check
cabal-gild -i eras/conway/impl/cardano-ledger-conway.cabal -m check
cabal-gild -i eras/conway/test-suite/cardano-ledger-conway-test.cabal -m check
cabal-gild -i eras/mary/impl/cardano-ledger-mary.cabal -m check
cabal-gild -i eras/shelley-ma/test-suite/cardano-ledger-shelley-ma-test.cabal -m check
cabal-gild -i eras/shelley/impl/cardano-ledger-shelley.cabal -m check
cabal-gild -i eras/shelley/test-suite/cardano-ledger-shelley-test.cabal -m check
cabal-gild -i libs/cardano-data/cardano-data.cabal -m check
cabal-gild -i libs/cardano-ledger-api/cardano-ledger-api.cabal -m check
cabal-gild -i libs/cardano-ledger-binary/cardano-ledger-binary.cabal -m check
cabal-gild -i libs/cardano-ledger-conformance/cardano-ledger-conformance.cabal -m check
cabal-gild -i libs/cardano-ledger-core/cardano-ledger-core.cabal -m check
cabal-gild -i libs/cardano-ledger-repl-environment/cardano-ledger-repl-environment.cabal -m check
cabal-gild -i libs/cardano-ledger-test/cardano-ledger-test.cabal -m check
cabal-gild -i libs/cardano-protocol-tpraos/cardano-protocol-tpraos.cabal -m check
cabal-gild -i libs/constrained-generators/constrained-generators.cabal -m check
cabal-gild -i libs/ledger-state/ledger-state.cabal -m check
cabal-gild -i libs/non-integral/non-integral.cabal -m check
cabal-gild -i libs/plutus-preprocessor/plutus-preprocessor.cabal -m check
cabal-gild -i libs/set-algebra/set-algebra.cabal -m check
cabal-gild -i libs/small-steps/small-steps.cabal -m check
cabal-gild -i libs/vector-map/vector-map.cabal -m check
