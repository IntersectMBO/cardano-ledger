{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Shelley.SoftForks (
  validMetadata,
  restrictPoolMetadataHash,
)
where

import Cardano.Ledger.BaseTypes (ProtVer (..), natVersion)

validMetadata ::
  ProtVer ->
  Bool
validMetadata pv = pv > ProtVer (natVersion @2) 0

restrictPoolMetadataHash ::
  ProtVer ->
  Bool
restrictPoolMetadataHash pv = pv > ProtVer (natVersion @4) 0
