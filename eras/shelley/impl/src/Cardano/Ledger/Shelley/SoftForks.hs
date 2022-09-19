{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Shelley.SoftForks
  ( validMetadata,
    restrictPoolMetadataHash,
  )
where

import Cardano.Ledger.BaseTypes (ProtVer (..))

validMetadata ::
  ProtVer ->
  Bool
validMetadata pv = pv > ProtVer 2 0

restrictPoolMetadataHash ::
  ProtVer ->
  Bool
restrictPoolMetadataHash pv = pv > ProtVer 4 0
