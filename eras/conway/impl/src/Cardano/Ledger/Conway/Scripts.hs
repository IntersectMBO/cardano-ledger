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
import Cardano.Ledger.Alonzo.Language (Language)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), isPlutusScript, translateAlonzoScript)
import Cardano.Ledger.Babbage.Scripts (babbageScriptPrefixTag)
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Data.ByteString.Short (ShortByteString)

type instance SomeScript 'PhaseOne (ConwayEra c) = Timelock (ConwayEra c)

type instance SomeScript 'PhaseTwo (ConwayEra c) = (Language, ShortByteString)

instance Crypto c => EraScript (ConwayEra c) where
  type Script (ConwayEra c) = AlonzoScript (ConwayEra c)

  upgradeScript = translateAlonzoScript

  scriptPrefixTag = babbageScriptPrefixTag

  phaseScript PhaseOneRep (TimelockScript s) = Just (Phase1Script s)
  phaseScript PhaseTwoRep (PlutusScript plutus) = Just (Phase2Script plutus)
  phaseScript _ _ = Nothing
