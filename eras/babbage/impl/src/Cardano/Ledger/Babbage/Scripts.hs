{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Figure 3: Functions related to scripts
--   Babbage Specification
module Cardano.Ledger.Babbage.Scripts (
  AlonzoScript (..),
  isPlutusScript,
  babbageScriptPrefixTag,
)
where

import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.Alonzo.Language
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoScript (..),
  isPlutusScript,
  translateAlonzoScript,
 )
import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Shelley.Scripts (nativeMultiSigTag)
import Data.ByteString (ByteString)

babbageScriptPrefixTag ::
  Script era ~ AlonzoScript era => Script era -> ByteString
babbageScriptPrefixTag script =
  case script of
    TimelockScript _ -> nativeMultiSigTag -- "\x00"
    PlutusScript (Plutus PlutusV1 _) -> "\x01"
    PlutusScript (Plutus PlutusV2 _) -> "\x02"
    PlutusScript (Plutus PlutusV3 _) -> "\x03"

instance Crypto c => EraScript (BabbageEra c) where
  type Script (BabbageEra c) = AlonzoScript (BabbageEra c)
  type NativeScript (BabbageEra c) = Timelock (BabbageEra c)

  upgradeScript = translateAlonzoScript

  scriptPrefixTag = babbageScriptPrefixTag

  getNativeScript = \case
    TimelockScript ts -> Just ts
    _ -> Nothing
