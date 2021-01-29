{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Shelley.Spec.Ledger.HardForks
  ( aggregatedRewards,
  )
where

import GHC.Records
import Shelley.Spec.Ledger.PParams (ProtVer (..))

aggregatedRewards ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
aggregatedRewards pp = pvMajor (getField @"_protocolVersion" pp) > 2
