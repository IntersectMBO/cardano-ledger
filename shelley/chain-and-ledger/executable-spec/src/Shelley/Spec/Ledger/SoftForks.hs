module Shelley.Spec.Ledger.SoftForks
  ( validMetaData,
  )
where

import Shelley.Spec.Ledger.PParams (PParams, PParams' (..), ProtVer (..))

validMetaData :: PParams era -> Bool
validMetaData pp = _protocolVersion pp > ProtVer 2 0
