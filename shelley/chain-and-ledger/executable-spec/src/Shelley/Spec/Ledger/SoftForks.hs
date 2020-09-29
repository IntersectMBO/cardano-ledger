module Shelley.Spec.Ledger.SoftForks
  ( validMetaData,
  )
where

import Shelley.Spec.Ledger.PParams (PParams, PParams' (..), ProtVer (..))

validMetaData :: PParams era -> Bool
validMetaData pp = pvMinor (_protocolVersion pp) > 0
