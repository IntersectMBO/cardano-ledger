{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Figure 3: Functions related to scripts
--   Babbage Specification
module Cardano.Ledger.Babbage.Scripts where

import Cardano.Ledger.Alonzo.Data
  ( AlonzoAuxiliaryData,
    hashAlonzoAuxiliaryData,
    validateAlonzoAuxiliaryData,
  )
import Cardano.Ledger.Alonzo.Language
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), isPlutusScript)
import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Shelley.Scripts (nativeMultiSigTag)

instance CC.Crypto c => EraScript (BabbageEra c) where
  type Script (BabbageEra c) = AlonzoScript (BabbageEra c)
  isNativeScript x = not (isPlutusScript x)
  scriptPrefixTag script =
    case script of
      (TimelockScript _) -> nativeMultiSigTag -- "\x00"
      (PlutusScript PlutusV1 _) -> "\x01"
      (PlutusScript PlutusV2 _) -> "\x02"

instance CC.Crypto c => EraAuxiliaryData (BabbageEra c) where
  type AuxiliaryData (BabbageEra c) = AlonzoAuxiliaryData (BabbageEra c)
  hashAuxiliaryData = hashAlonzoAuxiliaryData
  validateAuxiliaryData = validateAlonzoAuxiliaryData
