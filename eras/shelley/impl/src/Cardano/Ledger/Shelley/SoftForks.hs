{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Shelley.SoftForks (
  restrictPoolMetadataHash,
) where

import Cardano.Ledger.BaseTypes (ProtVer (..), natVersion)

restrictPoolMetadataHash ::
  ProtVer ->
  Bool
restrictPoolMetadataHash pv = pv > ProtVer (natVersion @4) 0
