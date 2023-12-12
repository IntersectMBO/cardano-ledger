{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Scripts (
  AlonzoScript (..),
  PlutusScript (..),
  isPlutusScript,
)
where

import Cardano.Ledger.Allegra.Scripts (Timelock, translateTimelock)
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (..),
  AlonzoScript (..),
  isPlutusScript,
 )
import Cardano.Ledger.Babbage.Scripts (PlutusScript (..))
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Plutus.Language
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.Scripts (nativeMultiSigTag)
import Control.DeepSeq (NFData (..), rwhnf)
import GHC.Generics
import NoThunks.Class (NoThunks (..))

instance Crypto c => EraScript (ConwayEra c) where
  type Script (ConwayEra c) = AlonzoScript (ConwayEra c)
  type NativeScript (ConwayEra c) = Timelock (ConwayEra c)

  upgradeScript = \case
    TimelockScript ts -> TimelockScript $ translateTimelock ts
    PlutusScript (BabbagePlutusV1 ps) -> PlutusScript $ ConwayPlutusV1 ps
    PlutusScript (BabbagePlutusV2 ps) -> PlutusScript $ ConwayPlutusV2 ps

  scriptPrefixTag = \case
    TimelockScript _ -> nativeMultiSigTag -- "\x00"
    PlutusScript (ConwayPlutusV1 _) -> "\x01"
    PlutusScript (ConwayPlutusV2 _) -> "\x02"
    PlutusScript (ConwayPlutusV3 _) -> "\x03"

  getNativeScript = \case
    TimelockScript ts -> Just ts
    _ -> Nothing

  fromNativeScript = TimelockScript

instance Crypto c => AlonzoEraScript (ConwayEra c) where
  data PlutusScript (ConwayEra c)
    = ConwayPlutusV1 !(Plutus 'PlutusV1)
    | ConwayPlutusV2 !(Plutus 'PlutusV2)
    | ConwayPlutusV3 !(Plutus 'PlutusV3)
    deriving (Eq, Ord, Show, Generic)

  eraMaxLanguage = PlutusV3

  mkPlutusScript plutus =
    case plutusSLanguage plutus of
      SPlutusV1 -> Just $ ConwayPlutusV1 plutus
      SPlutusV2 -> Just $ ConwayPlutusV2 plutus
      SPlutusV3 -> Just $ ConwayPlutusV3 plutus

  withPlutusScript (ConwayPlutusV1 plutus) f = f plutus
  withPlutusScript (ConwayPlutusV2 plutus) f = f plutus
  withPlutusScript (ConwayPlutusV3 plutus) f = f plutus

instance NFData (PlutusScript (ConwayEra c)) where
  rnf = rwhnf
instance NoThunks (PlutusScript (ConwayEra c))
instance Crypto c => SafeToHash (PlutusScript (ConwayEra c)) where
  originalBytes ps = withPlutusScript ps originalBytes
