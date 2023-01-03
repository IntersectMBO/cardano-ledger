module Cardano.Ledger.Api.Scripts.Data (
  -- * Alonzo and Babbage Era
  Data (Data),
  ScriptHash,
  hashData,
)
where

import Cardano.Ledger.Alonzo.Scripts.Data (Data (Data), hashData)
import Cardano.Ledger.Hashes (ScriptHash)
