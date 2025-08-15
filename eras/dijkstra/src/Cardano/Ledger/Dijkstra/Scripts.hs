{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Scripts (PlutusScript (..)) where

import Cardano.Ledger.Address (RewardAccount)
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
import Cardano.Ledger.Conway.Governance (ProposalProcedure, Voter)
import Cardano.Ledger.Conway.Scripts (
  ConwayEraScript (..),
  ConwayPlutusPurpose (..),
  PlutusScript (..),
 )
import Cardano.Ledger.Core (EraScript (..), EraTxCert (..), SafeToHash (..), ScriptHash)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams ()
import Cardano.Ledger.Dijkstra.TxCert ()
import Cardano.Ledger.Mary.Value (PolicyID)
import Cardano.Ledger.Plutus (Language (..), Plutus, SLanguage (..), plutusSLanguage)
import Cardano.Ledger.Shelley.Scripts (ShelleyEraScript (..))
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData (..), rwhnf)
import Data.MemPack (MemPack (..), packTagM, packedTagByteCount, unknownTagM, unpackTagM)
import Data.Word (Word32)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

data DijkstraPlutusPurpose f era
  = DijkstraSpending !(f Word32 TxIn)
  | DijkstraMinting !(f Word32 PolicyID)
  | DijkstraCertifying !(f Word32 (TxCert era))
  | DijkstraRewarding !(f Word32 RewardAccount)
  | DijkstraVoting !(f Word32 Voter)
  | DijkstraProposing !(f Word32 (ProposalProcedure era))
  | DijkstraGuarding !(f Word32 ScriptHash)
  deriving (Generic)

instance EraScript DijkstraEra where
  type Script DijkstraEra = AlonzoScript DijkstraEra
  type NativeScript DijkstraEra = Timelock DijkstraEra

  upgradeScript = \case
    TimelockScript ts -> TimelockScript $ translateTimelock ts
    PlutusScript (ConwayPlutusV1 s) -> PlutusScript $ DijkstraPlutusV1 s
    PlutusScript (ConwayPlutusV2 s) -> PlutusScript $ DijkstraPlutusV2 s
    PlutusScript (ConwayPlutusV3 s) -> PlutusScript $ DijkstraPlutusV3 s

  scriptPrefixTag = alonzoScriptPrefixTag

  getNativeScript (TimelockScript ts) = Just ts
  getNativeScript _ = Nothing

  fromNativeScript = TimelockScript

instance MemPack (PlutusScript DijkstraEra) where
  packedByteCount = \case
    DijkstraPlutusV1 script -> packedTagByteCount + packedByteCount script
    DijkstraPlutusV2 script -> packedTagByteCount + packedByteCount script
    DijkstraPlutusV3 script -> packedTagByteCount + packedByteCount script
    DijkstraPlutusV4 script -> packedTagByteCount + packedByteCount script
  packM = \case
    DijkstraPlutusV1 script -> packTagM 0 >> packM script
    DijkstraPlutusV2 script -> packTagM 1 >> packM script
    DijkstraPlutusV3 script -> packTagM 2 >> packM script
    DijkstraPlutusV4 script -> packTagM 3 >> packM script
  {-# INLINE packM #-}
  unpackM =
    unpackTagM >>= \case
      0 -> DijkstraPlutusV1 <$> unpackM
      1 -> DijkstraPlutusV2 <$> unpackM
      2 -> DijkstraPlutusV3 <$> unpackM
      3 -> DijkstraPlutusV4 <$> unpackM
      n -> unknownTagM @(PlutusScript DijkstraEra) n
  {-# INLINE unpackM #-}

instance NFData (PlutusScript DijkstraEra) where
  rnf = rwhnf

instance NoThunks (PlutusScript DijkstraEra)

instance SafeToHash (PlutusScript DijkstraEra) where
  originalBytes ps = withPlutusScript ps originalBytes

instance AlonzoEraScript DijkstraEra where
  data PlutusScript DijkstraEra
    = DijkstraPlutusV1 !(Plutus 'PlutusV1)
    | DijkstraPlutusV2 !(Plutus 'PlutusV2)
    | DijkstraPlutusV3 !(Plutus 'PlutusV3)
    | DijkstraPlutusV4 !(Plutus 'PlutusV4)
    deriving (Eq, Ord, Show, Generic)

  type PlutusPurpose f DijkstraEra = ConwayPlutusPurpose f DijkstraEra

  eraMaxLanguage = PlutusV3

  mkPlutusScript plutus =
    case plutusSLanguage plutus of
      SPlutusV1 -> pure $ DijkstraPlutusV1 plutus
      SPlutusV2 -> pure $ DijkstraPlutusV2 plutus
      SPlutusV3 -> pure $ DijkstraPlutusV3 plutus
      SPlutusV4 -> pure $ DijkstraPlutusV4 plutus

  withPlutusScript (DijkstraPlutusV1 plutus) f = f plutus
  withPlutusScript (DijkstraPlutusV2 plutus) f = f plutus
  withPlutusScript (DijkstraPlutusV3 plutus) f = f plutus
  withPlutusScript (DijkstraPlutusV4 plutus) f = f plutus

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
