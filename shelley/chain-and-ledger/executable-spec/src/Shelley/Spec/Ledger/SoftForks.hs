module Shelley.Spec.Ledger.SoftForks
  ( validMetadata,
  )
where

import Shelley.Spec.Ledger.PParams (PParams, PParams' (..), ProtVer (..))

validMetadata :: PParams era -> Bool
validMetadata pp = _protocolVersion pp > ProtVer 2 0
