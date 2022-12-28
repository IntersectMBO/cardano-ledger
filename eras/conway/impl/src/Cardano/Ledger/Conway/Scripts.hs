{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Scripts (
  AlonzoScript (..),
  isPlutusScript,
  babbageScriptPrefixTag,
)
where

import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.Alonzo.Data (
  AlonzoTxAuxData,
  hashAlonzoTxAuxData,
  validateAlonzoTxAuxData,
 )
import Cardano.Ledger.Alonzo.Language (Language)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), isPlutusScript)
import Cardano.Ledger.Babbage.Scripts (babbageScriptPrefixTag)
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC
import Data.ByteString.Short (ShortByteString)

type instance SomeScript 'PhaseOne (ConwayEra c) = Timelock (ConwayEra c)

type instance SomeScript 'PhaseTwo (ConwayEra c) = (Language, ShortByteString)

instance CC.Crypto c => EraScript (ConwayEra c) where
  type Script (ConwayEra c) = AlonzoScript (ConwayEra c)
  scriptPrefixTag = babbageScriptPrefixTag
  phaseScript PhaseOneRep (TimelockScript s) = Just (Phase1Script s)
  phaseScript PhaseTwoRep (PlutusScript lang bytes) = Just (Phase2Script lang bytes)
  phaseScript _ _ = Nothing

instance CC.Crypto c => EraTxAuxData (ConwayEra c) where
  type TxAuxData (ConwayEra c) = AlonzoTxAuxData (ConwayEra c)
  hashTxAuxData = hashAlonzoTxAuxData
  validateTxAuxData = validateAlonzoTxAuxData
