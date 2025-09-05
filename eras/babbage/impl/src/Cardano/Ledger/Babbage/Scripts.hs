{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Figure 3: Functions related to scripts
--   Babbage Specification
module Cardano.Ledger.Babbage.Scripts (
  AlonzoScript (..),
  isPlutusScript,
  PlutusScript (..),
) where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoPlutusPurpose (..),
  AlonzoScript (..),
  PlutusScript (..),
  alonzoScriptPrefixTag,
  eraUnsupportedLanguage,
  isPlutusScript,
 )
import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.Babbage.TxCert ()
import Cardano.Ledger.Plutus.Language
import Cardano.Ledger.Shelley.Scripts (ShelleyEraScript (..))
import Control.DeepSeq (NFData (..), rwhnf)
import Data.MemPack
import GHC.Generics
import NoThunks.Class (NoThunks (..))

instance EraScript BabbageEra where
  type Script BabbageEra = AlonzoScript BabbageEra
  type NativeScript BabbageEra = Timelock BabbageEra

  upgradeScript = \case
    NativeScript ts -> NativeScript $ translateTimelock ts
    PlutusScript (AlonzoPlutusV1 ps) -> PlutusScript $ BabbagePlutusV1 ps

  scriptPrefixTag = alonzoScriptPrefixTag

  getNativeScript = \case
    NativeScript ts -> Just ts
    _ -> Nothing

  fromNativeScript = NativeScript

instance AlonzoEraScript BabbageEra where
  data PlutusScript BabbageEra
    = BabbagePlutusV1 !(Plutus 'PlutusV1)
    | BabbagePlutusV2 !(Plutus 'PlutusV2)
    deriving (Eq, Ord, Show, Generic)
  type PlutusPurpose f BabbageEra = AlonzoPlutusPurpose f BabbageEra

  eraMaxLanguage = PlutusV2

  mkPlutusScript plutus =
    case plutusSLanguage plutus of
      SPlutusV1 -> pure $ BabbagePlutusV1 plutus
      SPlutusV2 -> pure $ BabbagePlutusV2 plutus
      slang -> eraUnsupportedLanguage @BabbageEra slang

  withPlutusScript (BabbagePlutusV1 plutus) f = f plutus
  withPlutusScript (BabbagePlutusV2 plutus) f = f plutus

  hoistPlutusPurpose f = \case
    AlonzoSpending x -> AlonzoSpending $ f x
    AlonzoMinting x -> AlonzoMinting $ f x
    AlonzoCertifying x -> AlonzoCertifying $ f x
    AlonzoRewarding x -> AlonzoRewarding $ f x

  mkSpendingPurpose = AlonzoSpending

  toSpendingPurpose (AlonzoSpending i) = Just i
  toSpendingPurpose _ = Nothing

  mkMintingPurpose = AlonzoMinting

  toMintingPurpose (AlonzoMinting i) = Just i
  toMintingPurpose _ = Nothing

  mkCertifyingPurpose = AlonzoCertifying

  toCertifyingPurpose (AlonzoCertifying i) = Just i
  toCertifyingPurpose _ = Nothing

  mkRewardingPurpose = AlonzoRewarding

  toRewardingPurpose (AlonzoRewarding i) = Just i
  toRewardingPurpose _ = Nothing

  upgradePlutusPurposeAsIx = \case
    AlonzoMinting (AsIx ix) -> AlonzoMinting (AsIx ix)
    AlonzoSpending (AsIx ix) -> AlonzoSpending (AsIx ix)
    AlonzoRewarding (AsIx ix) -> AlonzoRewarding (AsIx ix)
    AlonzoCertifying (AsIx ix) -> AlonzoCertifying (AsIx ix)

instance ShelleyEraScript BabbageEra where
  mkRequireSignature = mkRequireSignatureTimelock
  getRequireSignature = getRequireSignatureTimelock

  mkRequireAllOf = mkRequireAllOfTimelock
  getRequireAllOf = getRequireAllOfTimelock

  mkRequireAnyOf = mkRequireAnyOfTimelock
  getRequireAnyOf = getRequireAnyOfTimelock

  mkRequireMOf = mkRequireMOfTimelock
  getRequireMOf = getRequireMOfTimelock

instance AllegraEraScript BabbageEra where
  mkTimeStart = mkTimeStartTimelock
  getTimeStart = getTimeStartTimelock

  mkTimeExpire = mkTimeExpireTimelock
  getTimeExpire = getTimeExpireTimelock

  upgradeNativeScript = translateTimelock

instance NFData (PlutusScript BabbageEra) where
  rnf = rwhnf

instance NoThunks (PlutusScript BabbageEra)

instance SafeToHash (PlutusScript BabbageEra) where
  originalBytes ps = withPlutusScript ps originalBytes

instance MemPack (PlutusScript BabbageEra) where
  packedByteCount = \case
    BabbagePlutusV1 script -> packedTagByteCount + packedByteCount script
    BabbagePlutusV2 script -> packedTagByteCount + packedByteCount script
  packM = \case
    BabbagePlutusV1 script -> packTagM 0 >> packM script
    BabbagePlutusV2 script -> packTagM 1 >> packM script
  {-# INLINE packM #-}
  unpackM =
    unpackTagM >>= \case
      0 -> BabbagePlutusV1 <$> unpackM
      1 -> BabbagePlutusV2 <$> unpackM
      n -> unknownTagM @(PlutusScript BabbageEra) n
  {-# INLINE unpackM #-}
