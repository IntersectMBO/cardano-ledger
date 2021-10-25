{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Shelley.SoftForks
  ( validMetadata,
    restrictPoolMetadataHash,
  )
where

import Cardano.Ledger.BaseTypes (ProtVer (..))
import GHC.Records

validMetadata ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
validMetadata pp = getField @"_protocolVersion" pp > ProtVer 2 0

restrictPoolMetadataHash ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
restrictPoolMetadataHash pp = getField @"_protocolVersion" pp > ProtVer 4 0
