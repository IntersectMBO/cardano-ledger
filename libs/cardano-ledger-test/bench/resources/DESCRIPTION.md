This directory contains sample encoded resources for benchmarking purposes. They
are indexed by number.

# Example 0

Shelley era ledger state and transaction for benchmarking of transaction
application.

## Generation
These examples were generated using the following script (executed in `cabal repl cardano-ledger-shelley-test`)

```
:set -XTypeApplications

import Data.Proxy

import Cardano.Binary
import Control.State.Transition.Trace
import Control.State.Transition.Trace.Generator.QuickCheck
import qualified Data.ByteString as BS
import Cardano.Ledger.Shelley.Rules.Ledger
import Cardano.Ledger.Shelley
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes

import Test.Cardano.Ledger.Shelley.Utils
import Test.Cardano.Ledger.Shelley.Generator.Presets
import Test.Cardano.Ledger.Shelley.Generator.Trace.Ledger ()
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Cardano.Ledger.Shelley.PParams (PParams'(..))

import Test.QuickCheck (generate)

import Test.Cardano.Ledger.Shelley.Generator.Trace.Ledger

let ge = genEnv (Proxy @(ShelleyEra C_Crypto))
initLs <- generate $ mkGenesisLedgerState @([LedgerPredicateFailure (ShelleyEra C_Crypto)]) ge undefined
tr <- generate $ traceFromInitState @(LEDGER (ShelleyEra C_Crypto)) testGlobals 20 ge (Just $ \_ -> pure initLs)

let sst = last $ sourceSignalTargets tr
BS.writeFile "/tmp/0_ledgerstate.cbor" $ serialize' (source sst)
BS.writeFile "/tmp/0_tx.cbor" $ serialize' (signal sst)
```
```
:set -XTypeApplications

import Data.Proxy

import Cardano.Binary
import Control.State.Transition.Trace
import Control.State.Transition.Trace.Generator.QuickCheck
import qualified Data.ByteString as BS
import Cardano.Ledger.Shelley.Rules.Ledger
import Cardano.Ledger.Shelley
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes

import Test.Cardano.Ledger.Shelley.Utils
import Test.Cardano.Ledger.Shelley.Generator.Presets
import Test.Cardano.Ledger.Shelley.Generator.Trace.Ledger ()
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Cardano.Ledger.Shelley.PParams (PParams'(..))
import Test.QuickCheck (generate)

import Test.Cardano.Ledger.Shelley.Generator.Trace.Ledger

import Cardano.Ledger.Allegra
import Test.Cardano.Ledger.AllegraEraGen ()


let ge = genEnv (Proxy @(AllegraEra C_Crypto))
initLs <- generate $ mkGenesisLedgerState @([LedgerPredicateFailure (AllegraEra C_Crypto)]) ge undefined
tr <- generate $ traceFromInitState @(LEDGER (AllegraEra C_Crypto)) testGlobals 20 ge (Just $ \_ -> pure initLs)

let sst = last $ sourceSignalTargets tr
BS.writeFile "/tmp/0_ledgerstate.cbor" $ serialize' (source sst)
BS.writeFile "/tmp/0_tx.cbor" $ serialize' (signal sst)
```

# Example 2

Dijkstra era ledger state and two transactions of the maximal mainnet size (16384 bytes), used by
`Bench.Cardano.Ledger.Dijkstra.PoolReRegistration` (in the `bench` suite) and by the
`pool-reregistration-report` benchmark to measure the cost of decoding and applying BLS keys in stake pool re-registrations:

* `2_ledgerstate.cbor`: the slot and the `NewEpochState` that both transactions apply to, with one pool registered with a BLS key.
* `2_tx_bls.cbor`: re-registers the pool above 61 times, each time with a fresh BLS key and otherwise
  the smallest possible certificate (no owners, relays, metadata, etc).
* `2_tx_no_bls.cbor`: the control/baseling, re-registers that same pool 140 times without a BLS key.

The transactions are encoded as they are stored in a block.

## Generation

Regen with:

```
cabal repl cardano-ledger-test:bench:bench
ghci> import Bench.Cardano.Ledger.Dijkstra.PoolReRegistration.Gen
ghci> writePoolReRegistrationResources
```
