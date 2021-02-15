This directory contains sample encoded resources for benchmarking purposes. They
are indexed by number.

# Example 0

Shelley era ledger state and transaction for benchmarking of transaction
application.

## Generation
These examples were generated using the following script (executed in `cabal repl shelley-spec-ledger-test`)

```
:set -XTypeApplications

import Data.Proxy

import Cardano.Binary
import Control.State.Transition.Trace
import Control.State.Transition.Trace.Generator.QuickCheck
import qualified Data.ByteString as BS
import Shelley.Spec.Ledger.STS.Ledger
import Cardano.Ledger.Shelley
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes

import Test.Shelley.Spec.Ledger.Utils
import Test.Shelley.Spec.Ledger.Generator.Presets
import Test.Shelley.Spec.Ledger.Generator.Trace.Ledger ()
import Test.Shelley.Spec.Ledger.Generator.ShelleyEraGen ()
import Shelley.Spec.Ledger.PParams (PParams'(..))

import Test.QuickCheck (generate)

import Test.Shelley.Spec.Ledger.Generator.Trace.Ledger

let ge = genEnv (Proxy @(ShelleyEra C_Crypto))
initLs <- generate $ mkGenesisLedgerState @([[LedgerPredicateFailure (ShelleyEra C_Crypto)]]) ge undefined
tr <- generate $ traceFromInitState @(LEDGER (ShelleyEra C_Crypto)) testGlobals 20 ge (Just $ \_ -> pure initLs)

let sst = last $ sourceSignalTargets tr
BS.writeFile "/tmp/0_ledgerstate/cbor" $ serialize' (source sst)
BS.writeFile "/tmp/0_ledgerstate/cbor" $ serialize' (signal sst)
```
