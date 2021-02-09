{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Shelley.Spec.Ledger.SoftForks
  ( validMetadata,
  )
where

import GHC.Records
import Shelley.Spec.Ledger.PParams (ProtVer (..))

validMetadata ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
validMetadata pp = getField @"_protocolVersion" pp > ProtVer 2 0
