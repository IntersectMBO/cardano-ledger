module Shelley.Spec.Ledger.HardForks
  ( aggregatedRewards,
  )
where

import Shelley.Spec.Ledger.PParams (PParams, PParams' (..), ProtVer (..))

aggregatedRewards :: PParams era -> Bool
aggregatedRewards pp = pvMajor (_protocolVersion pp) > 2
