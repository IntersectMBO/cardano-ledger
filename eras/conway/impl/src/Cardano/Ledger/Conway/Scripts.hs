{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Scripts (
  AlonzoScript (..),
  isPlutusScript,
  babbageScriptPrefixTag,
)
where

import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoScript (..),
  isPlutusScript,
  translateAlonzoScript,
 )
import Cardano.Ledger.Babbage.Scripts (babbageScriptPrefixTag)
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto

instance Crypto c => EraScript (ConwayEra c) where
  type Script (ConwayEra c) = AlonzoScript (ConwayEra c)
  type NativeScript (ConwayEra c) = Timelock (ConwayEra c)

  upgradeScript = translateAlonzoScript

  scriptPrefixTag = babbageScriptPrefixTag

  getNativeScript = \case
    TimelockScript ts -> Just ts
    _ -> Nothing
