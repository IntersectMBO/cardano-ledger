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
  (HasField "sppProtocolVersion" pp ProtVer) =>
  pp ->
  Bool
validMetadata pp = getField @"sppProtocolVersion" pp > ProtVer 2 0

restrictPoolMetadataHash ::
  (HasField "sppProtocolVersion" pp ProtVer) =>
  pp ->
  Bool
restrictPoolMetadataHash pp = getField @"sppProtocolVersion" pp > ProtVer 4 0
