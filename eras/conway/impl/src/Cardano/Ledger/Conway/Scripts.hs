{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Scripts (
  ConwayEraScript (..),
  AlonzoScript (..),
  PlutusScript (..),
  isPlutusScript,
  ConwayPlutusPurpose (..),
#if __GLASGOW_HASKELL__ >= 914
  data VotingPurpose,
  data ProposingPurpose,
#else
  pattern VotingPurpose,
  pattern ProposingPurpose,
#endif
) where

import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoPlutusPurpose (..),
  AlonzoScript (..),
  alonzoScriptPrefixTag,
  eraUnsupportedLanguage,
  isPlutusScript,
 )
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Scripts (PlutusScript (..))
import Cardano.Ledger.BaseTypes (kindObject)
import Cardano.Ledger.Binary (
  CBORGroup (..),
  DecCBOR (decCBOR),
  DecCBORGroup (..),
  EncCBOR (..),
  EncCBORGroup (..),
  decodeWord8,
  encodeWord8,
 )
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Conway.Governance.Procedures
import Cardano.Ledger.Conway.TxCert ()
import Cardano.Ledger.Mary.Value (PolicyID)
import Cardano.Ledger.Plutus.Language
import Cardano.Ledger.Shelley.Scripts (ShelleyEraScript (..))
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData (..), rwhnf)
import Data.Aeson (ToJSON (..), (.=))
import Data.MemPack
import Data.Typeable
import Data.Word (Word32)
import GHC.Generics
import NoThunks.Class (NoThunks (..))

class AlonzoEraScript era => ConwayEraScript era where
  mkVotingPurpose :: f Word32 Voter -> PlutusPurpose f era

  toVotingPurpose :: PlutusPurpose f era -> Maybe (f Word32 Voter)

  mkProposingPurpose :: f Word32 (ProposalProcedure era) -> PlutusPurpose f era

  toProposingPurpose :: PlutusPurpose f era -> Maybe (f Word32 (ProposalProcedure era))

instance EraScript ConwayEra where
  type Script ConwayEra = AlonzoScript ConwayEra
  type NativeScript ConwayEra = Timelock ConwayEra

  upgradeScript = \case
    NativeScript ts -> NativeScript $ translateTimelock ts
    PlutusScript (BabbagePlutusV1 ps) -> PlutusScript $ ConwayPlutusV1 ps
    PlutusScript (BabbagePlutusV2 ps) -> PlutusScript $ ConwayPlutusV2 ps

  scriptPrefixTag = alonzoScriptPrefixTag

  getNativeScript = \case
    NativeScript ts -> Just ts
    _ -> Nothing

  fromNativeScript = NativeScript

instance AlonzoEraScript ConwayEra where
  data PlutusScript ConwayEra
    = ConwayPlutusV1 !(Plutus 'PlutusV1)
    | ConwayPlutusV2 !(Plutus 'PlutusV2)
    | ConwayPlutusV3 !(Plutus 'PlutusV3)
    deriving (Eq, Ord, Show, Generic)

  type PlutusPurpose f ConwayEra = ConwayPlutusPurpose f ConwayEra

  eraMaxLanguage = PlutusV3

  mkPlutusScript plutus =
    case plutusSLanguage plutus of
      SPlutusV1 -> pure $ ConwayPlutusV1 plutus
      SPlutusV2 -> pure $ ConwayPlutusV2 plutus
      SPlutusV3 -> pure $ ConwayPlutusV3 plutus
      slang -> eraUnsupportedLanguage @ConwayEra slang

  withPlutusScript (ConwayPlutusV1 plutus) f = f plutus
  withPlutusScript (ConwayPlutusV2 plutus) f = f plutus
  withPlutusScript (ConwayPlutusV3 plutus) f = f plutus

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
    AlonzoSpending (AsIx ix) -> ConwaySpending (AsIx ix)
    AlonzoMinting (AsIx ix) -> ConwayMinting (AsIx ix)
    AlonzoCertifying (AsIx ix) -> ConwayCertifying (AsIx ix)
    AlonzoRewarding (AsIx ix) -> ConwayRewarding (AsIx ix)

instance ConwayEraScript ConwayEra where
  mkVotingPurpose = ConwayVoting

  toVotingPurpose (ConwayVoting i) = Just i
  toVotingPurpose _ = Nothing

  mkProposingPurpose = ConwayProposing

  toProposingPurpose (ConwayProposing i) = Just i
  toProposingPurpose _ = Nothing

instance ShelleyEraScript ConwayEra where
  mkRequireSignature = mkRequireSignatureTimelock
  getRequireSignature = getRequireSignatureTimelock

  mkRequireAllOf = mkRequireAllOfTimelock
  getRequireAllOf = getRequireAllOfTimelock

  mkRequireAnyOf = mkRequireAnyOfTimelock
  getRequireAnyOf = getRequireAnyOfTimelock

  mkRequireMOf = mkRequireMOfTimelock
  getRequireMOf = getRequireMOfTimelock

instance AllegraEraScript ConwayEra where
  mkTimeStart = mkTimeStartTimelock
  getTimeStart = getTimeStartTimelock

  mkTimeExpire = mkTimeExpireTimelock
  getTimeExpire = getTimeExpireTimelock

instance NFData (PlutusScript ConwayEra) where
  rnf = rwhnf

instance NoThunks (PlutusScript ConwayEra)

instance SafeToHash (PlutusScript ConwayEra) where
  originalBytes ps = withPlutusScript ps originalBytes

instance MemPack (PlutusScript ConwayEra) where
  packedByteCount = \case
    ConwayPlutusV1 script -> packedTagByteCount + packedByteCount script
    ConwayPlutusV2 script -> packedTagByteCount + packedByteCount script
    ConwayPlutusV3 script -> packedTagByteCount + packedByteCount script
  packM = \case
    ConwayPlutusV1 script -> packTagM 0 >> packM script
    ConwayPlutusV2 script -> packTagM 1 >> packM script
    ConwayPlutusV3 script -> packTagM 2 >> packM script
  {-# INLINE packM #-}
  unpackM =
    unpackTagM >>= \case
      0 -> ConwayPlutusV1 <$> unpackM
      1 -> ConwayPlutusV2 <$> unpackM
      2 -> ConwayPlutusV3 <$> unpackM
      n -> unknownTagM @(PlutusScript ConwayEra) n
  {-# INLINE unpackM #-}

data ConwayPlutusPurpose f era
  = ConwaySpending !(f Word32 TxIn)
  | ConwayMinting !(f Word32 PolicyID)
  | ConwayCertifying !(f Word32 (TxCert era))
  | ConwayRewarding !(f Word32 RewardAccount)
  | ConwayVoting !(f Word32 Voter)
  | ConwayProposing !(f Word32 (ProposalProcedure era))
  deriving (Generic)

deriving instance Eq (ConwayPlutusPurpose AsIx era)

deriving instance Ord (ConwayPlutusPurpose AsIx era)

deriving instance Show (ConwayPlutusPurpose AsIx era)

instance NoThunks (ConwayPlutusPurpose AsIx era)

deriving instance (Eq (TxCert era), EraPParams era) => Eq (ConwayPlutusPurpose AsItem era)

deriving instance (Show (TxCert era), EraPParams era) => Show (ConwayPlutusPurpose AsItem era)

instance (NoThunks (TxCert era), EraPParams era) => NoThunks (ConwayPlutusPurpose AsItem era)

deriving via
  (CBORGroup (ConwayPlutusPurpose f era))
  instance
    ( forall a b. (EncCBOR a, EncCBOR b) => EncCBOR (f a b)
    , EraPParams era
    , EncCBOR (TxCert era)
    ) =>
    EncCBOR (ConwayPlutusPurpose f era)

deriving via
  (CBORGroup (ConwayPlutusPurpose f era))
  instance
    ( forall a b. (EncCBOR a, EncCBOR b) => EncCBOR (f a b)
    , forall a b. (DecCBOR a, DecCBOR b) => DecCBOR (f a b)
    , EraPParams era
    , Typeable f
    , EncCBOR (TxCert era)
    , DecCBOR (TxCert era)
    ) =>
    DecCBOR (ConwayPlutusPurpose f era)

deriving instance (Eq (TxCert era), EraPParams era) => Eq (ConwayPlutusPurpose AsIxItem era)

deriving instance (Show (TxCert era), EraPParams era) => Show (ConwayPlutusPurpose AsIxItem era)

instance (NoThunks (TxCert era), EraPParams era) => NoThunks (ConwayPlutusPurpose AsIxItem era)

instance
  (forall a b. (NFData a, NFData b) => NFData (f a b), NFData (TxCert era), EraPParams era) =>
  NFData (ConwayPlutusPurpose f era)
  where
  rnf = \case
    ConwaySpending x -> rnf x
    ConwayMinting x -> rnf x
    ConwayCertifying x -> rnf x
    ConwayRewarding x -> rnf x
    ConwayVoting x -> rnf x
    ConwayProposing x -> rnf x

instance
  ( forall a b. (EncCBOR a, EncCBOR b) => EncCBOR (f a b)
  , EraPParams era
  , EncCBOR (TxCert era)
  ) =>
  EncCBORGroup (ConwayPlutusPurpose f era)
  where
  listLen _ = 2
  listLenBound _ = 2
  encCBORGroup = \case
    ConwaySpending p -> encodeWord8 0 <> encCBOR p
    ConwayMinting p -> encodeWord8 1 <> encCBOR p
    ConwayCertifying p -> encodeWord8 2 <> encCBOR p
    ConwayRewarding p -> encodeWord8 3 <> encCBOR p
    ConwayVoting p -> encodeWord8 4 <> encCBOR p
    ConwayProposing p -> encodeWord8 5 <> encCBOR p

instance
  ( forall a b. (DecCBOR a, DecCBOR b) => DecCBOR (f a b)
  , EraPParams era
  , Typeable f
  , DecCBOR (TxCert era)
  ) =>
  DecCBORGroup (ConwayPlutusPurpose f era)
  where
  decCBORGroup =
    decodeWord8 >>= \case
      0 -> ConwaySpending <$> decCBOR
      1 -> ConwayMinting <$> decCBOR
      2 -> ConwayCertifying <$> decCBOR
      3 -> ConwayRewarding <$> decCBOR
      4 -> ConwayVoting <$> decCBOR
      5 -> ConwayProposing <$> decCBOR
      n -> fail $ "Unexpected tag for ConwayPlutusPurpose: " <> show n

instance
  ( forall a b. (ToJSON a, ToJSON b) => ToJSON (f a b)
  , ToJSON (TxCert era)
  , EraPParams era
  ) =>
  ToJSON (ConwayPlutusPurpose f era)
  where
  toJSON = \case
    ConwaySpending n -> kindObjectWithValue "ConwaySpending" n
    ConwayMinting n -> kindObjectWithValue "ConwayMinting" n
    ConwayCertifying n -> kindObjectWithValue "ConwayCertifying" n
    ConwayRewarding n -> kindObjectWithValue "ConwayRewarding" n
    ConwayVoting n -> kindObjectWithValue "ConwayVoting" n
    ConwayProposing n -> kindObjectWithValue "ConwayProposing" n
    where
      kindObjectWithValue name n = kindObject name ["value" .= n]

pattern VotingPurpose ::
  ConwayEraScript era => f Word32 Voter -> PlutusPurpose f era
pattern VotingPurpose c <- (toVotingPurpose -> Just c)
  where
    VotingPurpose c = mkVotingPurpose c

pattern ProposingPurpose ::
  ConwayEraScript era => f Word32 (ProposalProcedure era) -> PlutusPurpose f era
pattern ProposingPurpose c <- (toProposingPurpose -> Just c)
  where
    ProposingPurpose c = mkProposingPurpose c
