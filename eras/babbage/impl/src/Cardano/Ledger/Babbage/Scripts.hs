{-# LANGUAGE AllowAmbiguousTypes #-}
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
module Cardano.Ledger.Babbage.Scripts
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
import Cardano.Ledger.Alonzo.Language
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Shelley.Scripts (nativeMultiSigTag)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock)
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)

babbageScriptPrefixTag ::
  Script era ~ AlonzoScript era => Script era -> ByteString
babbageScriptPrefixTag script =
  case script of
    (TimelockScript _) -> nativeMultiSigTag -- "\x00"
    (PlutusScript PlutusV1 _) -> "\x01"
    (PlutusScript PlutusV2 _) -> "\x02"

type instance SomeScript 'PhaseOne (BabbageEra c) = Timelock (BabbageEra c)

type instance SomeScript 'PhaseTwo (BabbageEra c) = (Language, ShortByteString)

instance CC.Crypto c => EraScript (BabbageEra c) where
  type Script (BabbageEra c) = AlonzoScript (BabbageEra c)
  scriptPrefixTag = babbageScriptPrefixTag
  phaseScript PhaseOneRep (TimelockScript s) = Just (Phase1Script s)
  phaseScript PhaseTwoRep (PlutusScript lang bytes) = Just (Phase2Script lang bytes)
  phaseScript _ _ = Nothing

isPlutusScript :: forall era. EraScript era => Script era -> Bool
isPlutusScript x =
  case phaseScript @era PhaseTwoRep x of
    Just _ -> True
    Nothing -> False

instance CC.Crypto c => EraAuxiliaryData (BabbageEra c) where
  type AuxiliaryData (BabbageEra c) = AlonzoAuxiliaryData (BabbageEra c)
  hashAuxiliaryData = hashAlonzoAuxiliaryData
  validateAuxiliaryData = validateAlonzoAuxiliaryData
