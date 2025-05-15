{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Scripts () where

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
 )
import Cardano.Ledger.Alonzo (AlonzoScript)
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (..),
  AlonzoScript (..),
  alonzoScriptPrefixTag,
 )
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Scripts (ConwayEraScript (..), ConwayPlutusPurpose (..))
import Cardano.Ledger.Core (EraScript (..), SafeToHash)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams ()
import Cardano.Ledger.Dijkstra.TxCert ()
import Cardano.Ledger.Plutus (Language (..))
import Cardano.Ledger.Shelley.Scripts (ShelleyEraScript (..))
import Control.DeepSeq (NFData)
import Data.MemPack (MemPack (..))
import NoThunks.Class (NoThunks)

instance EraScript DijkstraEra where
  type Script DijkstraEra = AlonzoScript DijkstraEra
  type NativeScript DijkstraEra = Timelock DijkstraEra

  upgradeScript = undefined

  scriptPrefixTag = alonzoScriptPrefixTag

  getNativeScript (TimelockScript ts) = Just ts
  getNativeScript _ = Nothing

  fromNativeScript = TimelockScript

instance MemPack (PlutusScript DijkstraEra) where
  packedByteCount = undefined
  packM = undefined
  unpackM = undefined

instance AlonzoEraScript DijkstraEra where
  newtype PlutusScript DijkstraEra = MkDijkstraPlutusScript (PlutusScript ConwayEra)
    deriving newtype (SafeToHash, Show, NFData, NoThunks, Eq, Ord)

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

  upgradePlutusPurposeAsIx = undefined

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
