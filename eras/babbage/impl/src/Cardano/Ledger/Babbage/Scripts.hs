{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  PlutusScript (..),
)
where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoPlutusPurpose (..),
  AlonzoScript (..),
  PlutusScript (..),
  alonzoScriptPrefixTag,
  isPlutusScript,
 )
import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.Babbage.TxCert ()
import Cardano.Ledger.Crypto
import Cardano.Ledger.Plutus.Language
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.Scripts (ShelleyEraScript (..))
import Control.DeepSeq (NFData (..), rwhnf)
import GHC.Generics
import NoThunks.Class (NoThunks (..))

instance Crypto c => EraScript (BabbageEra c) where
  type Script (BabbageEra c) = AlonzoScript (BabbageEra c)
  type NativeScript (BabbageEra c) = Timelock (BabbageEra c)

  upgradeScript = \case
    TimelockScript ts -> TimelockScript $ translateTimelock ts
    PlutusScript (AlonzoPlutusV1 ps) -> PlutusScript $ BabbagePlutusV1 ps

  scriptPrefixTag = alonzoScriptPrefixTag

  getNativeScript = \case
    TimelockScript ts -> Just ts
    _ -> Nothing

  fromNativeScript = TimelockScript

instance Crypto c => AlonzoEraScript (BabbageEra c) where
  data PlutusScript (BabbageEra c)
    = BabbagePlutusV1 !(Plutus 'PlutusV1)
    | BabbagePlutusV2 !(Plutus 'PlutusV2)
    deriving (Eq, Ord, Show, Generic)
  type PlutusPurpose f (BabbageEra c) = AlonzoPlutusPurpose f (BabbageEra c)

  eraMaxLanguage = PlutusV2

  mkPlutusScript plutus =
    case plutusSLanguage plutus of
      SPlutusV1 -> Just $ BabbagePlutusV1 plutus
      SPlutusV2 -> Just $ BabbagePlutusV2 plutus
      _ -> Nothing

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

instance Crypto c => ShelleyEraScript (BabbageEra c) where
  {-# SPECIALIZE instance ShelleyEraScript (BabbageEra StandardCrypto) #-}

  mkRequireSignature = mkRequireSignatureTimelock
  getRequireSignature = getRequireSignatureTimelock

  mkRequireAllOf = mkRequireAllOfTimelock
  getRequireAllOf = getRequireAllOfTimelock

  mkRequireAnyOf = mkRequireAnyOfTimelock
  getRequireAnyOf = getRequireAnyOfTimelock

  mkRequireMOf = mkRequireMOfTimelock
  getRequireMOf = getRequireMOfTimelock

instance Crypto c => AllegraEraScript (BabbageEra c) where
  {-# SPECIALIZE instance AllegraEraScript (BabbageEra StandardCrypto) #-}

  mkTimeStart = mkTimeStartTimelock
  getTimeStart = getTimeStartTimelock

  mkTimeExpire = mkTimeExpireTimelock
  getTimeExpire = getTimeExpireTimelock

instance NFData (PlutusScript (BabbageEra c)) where
  rnf = rwhnf
instance NoThunks (PlutusScript (BabbageEra c))
instance Crypto c => SafeToHash (PlutusScript (BabbageEra c)) where
  originalBytes ps = withPlutusScript ps originalBytes
