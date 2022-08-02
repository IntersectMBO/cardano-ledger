{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Scripts
  ( AlonzoScript (..),
    isPlutusScript,
    babbageScriptPrefixTag,
  )
where

import Cardano.Ledger.Alonzo.Data
  ( AlonzoAuxiliaryData,
    hashAlonzoAuxiliaryData,
    validateAlonzoAuxiliaryData,
  )
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), isPlutusScript)
import Cardano.Ledger.Babbage.Scripts (babbageScriptPrefixTag)
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC

instance CC.Crypto c => EraScript (ConwayEra c) where
  type Script (ConwayEra c) = AlonzoScript (ConwayEra c)
  isNativeScript x = not (isPlutusScript x)
  scriptPrefixTag = babbageScriptPrefixTag

instance CC.Crypto c => EraAuxiliaryData (ConwayEra c) where
  type AuxiliaryData (ConwayEra c) = AlonzoAuxiliaryData (ConwayEra c)
  hashAuxiliaryData = hashAlonzoAuxiliaryData
  validateAuxiliaryData = validateAlonzoAuxiliaryData
