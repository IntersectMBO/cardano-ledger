{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Scripts (PlutusScript (..)) where

import Cardano.Ledger.Allegra.Scripts (
  AllegraEraScript (..),
  Timelock,
  getRequireAllOfTimelock,
  getRequireAnyOfTimelock,
  getRequireMOfTimelock,
  getRequireSignatureTimelock,
  getTimeExpireTimelock,
  getTimeStartTimelock,
  mkRequireAllOfTimelock,
  mkRequireAnyOfTimelock,
  mkRequireMOfTimelock,
  mkRequireSignatureTimelock,
  mkTimeExpireTimelock,
  mkTimeStartTimelock,
  translateTimelock,
 )
import Cardano.Ledger.Alonzo (AlonzoScript)
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (..),
  AlonzoScript (..),
  AsIx (..),
  alonzoScriptPrefixTag,
 )
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Scripts (
  ConwayEraScript (..),
  ConwayPlutusPurpose (..),
  PlutusScript (..),
 )
import Cardano.Ledger.Core (EraScript (..), SafeToHash)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams ()
import Cardano.Ledger.Dijkstra.TxCert ()
import Cardano.Ledger.Plutus (Language (..))
import Cardano.Ledger.Shelley.Scripts (ShelleyEraScript (..))
import Control.DeepSeq (NFData)
import Data.MemPack (MemPack (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

instance EraScript DijkstraEra where
  type Script DijkstraEra = AlonzoScript DijkstraEra
  type NativeScript DijkstraEra = Timelock DijkstraEra

  upgradeScript = \case
    TimelockScript ts -> TimelockScript $ translateTimelock ts
    PlutusScript ps -> PlutusScript $ MkDijkstraPlutusScript ps

  scriptPrefixTag = alonzoScriptPrefixTag

  getNativeScript (TimelockScript ts) = Just ts
  getNativeScript _ = Nothing

  fromNativeScript = TimelockScript

instance MemPack (PlutusScript DijkstraEra) where
  packedByteCount = packedByteCount . unDijkstraPlutusScript
  packM = packM . unDijkstraPlutusScript
  unpackM = MkDijkstraPlutusScript <$> unpackM

instance AlonzoEraScript DijkstraEra where
  newtype PlutusScript DijkstraEra = MkDijkstraPlutusScript
    {unDijkstraPlutusScript :: PlutusScript ConwayEra}
    deriving newtype (SafeToHash, Show, NFData, NoThunks, Eq, Ord, Generic)

  type PlutusPurpose f DijkstraEra = ConwayPlutusPurpose f DijkstraEra

  eraMaxLanguage = PlutusV3

  mkPlutusScript = fmap MkDijkstraPlutusScript . mkPlutusScript

  withPlutusScript (MkDijkstraPlutusScript s) = withPlutusScript s

  hoistPlutusPurpose f = \case
    ConwaySpending x -> ConwaySpending $ f x
    ConwayMinting x -> ConwayMinting $ f x
    ConwayCertifying x -> ConwayCertifying $ f x
    ConwayRewarding x -> ConwayRewarding $ f x
    ConwayVoting x -> ConwayVoting $ f x
    ConwayProposing x -> ConwayProposing $ f x

  mkSpendingPurpose = ConwaySpending

  toSpendingPurpose (ConwaySpending i) = Just i
  toSpendingPurpose _ = Nothing

  mkMintingPurpose = ConwayMinting

  toMintingPurpose (ConwayMinting i) = Just i
  toMintingPurpose _ = Nothing

  mkCertifyingPurpose = ConwayCertifying

  toCertifyingPurpose (ConwayCertifying i) = Just i
  toCertifyingPurpose _ = Nothing

  mkRewardingPurpose = ConwayRewarding

  toRewardingPurpose (ConwayRewarding i) = Just i
  toRewardingPurpose _ = Nothing

  upgradePlutusPurposeAsIx = \case
    ConwaySpending (AsIx ix) -> ConwaySpending (AsIx ix)
    ConwayMinting (AsIx ix) -> ConwayMinting (AsIx ix)
    ConwayCertifying (AsIx ix) -> ConwayCertifying (AsIx ix)
    ConwayRewarding (AsIx ix) -> ConwayRewarding (AsIx ix)
    ConwayVoting (AsIx ix) -> ConwayVoting (AsIx ix)
    ConwayProposing (AsIx ix) -> ConwayProposing (AsIx ix)

instance ConwayEraScript DijkstraEra where
  mkVotingPurpose = ConwayVoting

  toVotingPurpose (ConwayVoting i) = Just i
  toVotingPurpose _ = Nothing

  mkProposingPurpose = ConwayProposing

  toProposingPurpose (ConwayProposing i) = Just i
  toProposingPurpose _ = Nothing

instance ShelleyEraScript DijkstraEra where
  mkRequireSignature = mkRequireSignatureTimelock
  getRequireSignature = getRequireSignatureTimelock

  mkRequireAllOf = mkRequireAllOfTimelock
  getRequireAllOf = getRequireAllOfTimelock

  mkRequireAnyOf = mkRequireAnyOfTimelock
  getRequireAnyOf = getRequireAnyOfTimelock

  mkRequireMOf = mkRequireMOfTimelock
  getRequireMOf = getRequireMOfTimelock

instance AllegraEraScript DijkstraEra where
  mkTimeStart = mkTimeStartTimelock
  getTimeStart = getTimeStartTimelock

  mkTimeExpire = mkTimeExpireTimelock
  getTimeExpire = getTimeExpireTimelock
