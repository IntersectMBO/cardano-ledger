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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Scripts (
  BabelEraScript (..),
  AlonzoScript (..),
  PlutusScript (..),
  isPlutusScript,
  BabelPlutusPurpose (..),
  pattern VotingPurpose,
  pattern ProposingPurpose,
)
where

import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.Allegra.Scripts (Timelock, translateTimelock)
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoScript (..),
  alonzoScriptPrefixTag,
  isPlutusScript,
 )
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Scripts (PlutusScript (..))
import Cardano.Ledger.Babel.Era
import Cardano.Ledger.Babel.TxCert ()
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
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..), PlutusScript (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.Mary.Value (PolicyID)
import Cardano.Ledger.Plutus.Language
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData (..), rwhnf)
import Data.Aeson (ToJSON (..), (.=))
import Data.Typeable
import Data.Word (Word16, Word32, Word8)
import GHC.Generics
import NoThunks.Class (NoThunks (..))

class AlonzoEraScript era => BabelEraScript era where
  mkVotingPurpose :: f Word32 (Voter (EraCrypto era)) -> PlutusPurpose f era

  toVotingPurpose :: PlutusPurpose f era -> Maybe (f Word32 (Voter (EraCrypto era)))

  mkProposingPurpose :: f Word32 (ProposalProcedure era) -> PlutusPurpose f era

  toProposingPurpose :: PlutusPurpose f era -> Maybe (f Word32 (ProposalProcedure era))

instance Crypto c => EraScript (BabelEra c) where
  type Script (BabelEra c) = AlonzoScript (BabelEra c)
  type NativeScript (BabelEra c) = Timelock (BabelEra c)

  upgradeScript = \case
    TimelockScript ts -> TimelockScript $ translateTimelock ts
    PlutusScript (ConwayPlutusV1 ps) -> PlutusScript $ BabelPlutusV1 ps
    PlutusScript (ConwayPlutusV2 ps) -> PlutusScript $ BabelPlutusV2 ps
    PlutusScript (ConwayPlutusV3 ps) -> PlutusScript $ BabelPlutusV3 ps

  scriptPrefixTag = alonzoScriptPrefixTag

  getNativeScript = \case
    TimelockScript ts -> Just ts
    _ -> Nothing

  fromNativeScript = TimelockScript

instance Crypto c => AlonzoEraScript (BabelEra c) where
  data PlutusScript (BabelEra c)
    = BabelPlutusV1 !(Plutus 'PlutusV1)
    | BabelPlutusV2 !(Plutus 'PlutusV2)
    | BabelPlutusV3 !(Plutus 'PlutusV3)
    | BabelPlutusV4 !(Plutus 'PlutusV4)
    deriving (Eq, Ord, Show, Generic)

  type PlutusPurpose f (BabelEra c) = BabelPlutusPurpose f (BabelEra c)

  eraMaxLanguage = PlutusV3

  mkPlutusScript plutus =
    case plutusSLanguage plutus of
      SPlutusV1 -> Just $ BabelPlutusV1 plutus
      SPlutusV2 -> Just $ BabelPlutusV2 plutus
      SPlutusV3 -> Just $ BabelPlutusV3 plutus
      SPlutusV4 -> Just $ BabelPlutusV4 plutus

  withPlutusScript (BabelPlutusV1 plutus) f = f plutus
  withPlutusScript (BabelPlutusV2 plutus) f = f plutus
  withPlutusScript (BabelPlutusV3 plutus) f = f plutus
  withPlutusScript (BabelPlutusV4 plutus) f = f plutus

  hoistPlutusPurpose f = \case
    BabelSpending x -> BabelSpending $ f x
    BabelMinting x -> BabelMinting $ f x
    BabelCertifying x -> BabelCertifying $ f x
    BabelRewarding x -> BabelRewarding $ f x
    BabelVoting x -> BabelVoting $ f x
    BabelProposing x -> BabelProposing $ f x

  mkSpendingPurpose = BabelSpending

  toSpendingPurpose (BabelSpending i) = Just i
  toSpendingPurpose _ = Nothing

  mkMintingPurpose = BabelMinting

  toMintingPurpose (BabelMinting i) = Just i
  toMintingPurpose _ = Nothing

  mkCertifyingPurpose = BabelCertifying

  toCertifyingPurpose (BabelCertifying i) = Just i
  toCertifyingPurpose _ = Nothing

  mkRewardingPurpose = BabelRewarding

  toRewardingPurpose (BabelRewarding i) = Just i
  toRewardingPurpose _ = Nothing

  upgradePlutusPurposeAsIx = \case
    ConwaySpending (AsIx ix) -> BabelSpending (AsIx ix)
    ConwayMinting (AsIx ix) -> BabelMinting (AsIx ix)
    ConwayCertifying (AsIx ix) -> BabelCertifying (AsIx ix)
    ConwayRewarding (AsIx ix) -> BabelRewarding (AsIx ix)
    ConwayVoting (AsIx ix) -> BabelVoting (AsIx ix)
    ConwayProposing (AsIx ix) -> BabelProposing (AsIx ix)

instance Crypto c => BabelEraScript (BabelEra c) where
  mkVotingPurpose = BabelVoting

  toVotingPurpose (BabelVoting i) = Just i
  toVotingPurpose _ = Nothing

  mkProposingPurpose = BabelProposing

  toProposingPurpose (BabelProposing i) = Just i
  toProposingPurpose _ = Nothing

instance NFData (PlutusScript (BabelEra c)) where
  rnf = rwhnf
instance NoThunks (PlutusScript (BabelEra c))
instance Crypto c => SafeToHash (PlutusScript (BabelEra c)) where
  originalBytes ps = withPlutusScript ps originalBytes

data BabelPlutusPurpose f era
  = BabelSpending !(f Word32 (TxIn (EraCrypto era)))
  | BabelMinting !(f Word32 (PolicyID (EraCrypto era)))
  | BabelCertifying !(f Word32 (TxCert era))
  | BabelRewarding !(f Word32 (RewardAccount (EraCrypto era)))
  | BabelVoting !(f Word32 (Voter (EraCrypto era)))
  | BabelProposing !(f Word32 (ProposalProcedure era))
  deriving (Generic)

deriving instance Eq (BabelPlutusPurpose AsIx era)
deriving instance Ord (BabelPlutusPurpose AsIx era)
deriving instance Show (BabelPlutusPurpose AsIx era)
instance NoThunks (BabelPlutusPurpose AsIx era)

deriving instance (Eq (TxCert era), EraPParams era) => Eq (BabelPlutusPurpose AsItem era)
deriving instance (Show (TxCert era), EraPParams era) => Show (BabelPlutusPurpose AsItem era)
instance (NoThunks (TxCert era), EraPParams era) => NoThunks (BabelPlutusPurpose AsItem era)
deriving via
  (CBORGroup (BabelPlutusPurpose f era))
  instance
    ( forall a b. (EncCBOR a, EncCBOR b) => EncCBOR (f a b)
    , EraPParams era
    , Typeable f
    , EncCBOR (TxCert era)
    ) =>
    EncCBOR (BabelPlutusPurpose f era)
deriving via
  (CBORGroup (BabelPlutusPurpose f era))
  instance
    ( forall a b. (EncCBOR a, EncCBOR b) => EncCBOR (f a b)
    , forall a b. (DecCBOR a, DecCBOR b) => DecCBOR (f a b)
    , EraPParams era
    , Typeable f
    , EncCBOR (TxCert era)
    , DecCBOR (TxCert era)
    ) =>
    DecCBOR (BabelPlutusPurpose f era)

deriving instance (Eq (TxCert era), EraPParams era) => Eq (BabelPlutusPurpose AsIxItem era)
deriving instance (Show (TxCert era), EraPParams era) => Show (BabelPlutusPurpose AsIxItem era)
instance (NoThunks (TxCert era), EraPParams era) => NoThunks (BabelPlutusPurpose AsIxItem era)

instance
  (forall a b. (NFData a, NFData b) => NFData (f a b), NFData (TxCert era), EraPParams era) =>
  NFData (BabelPlutusPurpose f era)
  where
  rnf = \case
    BabelSpending x -> rnf x
    BabelMinting x -> rnf x
    BabelCertifying x -> rnf x
    BabelRewarding x -> rnf x
    BabelVoting x -> rnf x
    BabelProposing x -> rnf x

instance
  ( forall a b. (EncCBOR a, EncCBOR b) => EncCBOR (f a b)
  , EraPParams era
  , Typeable f
  , EncCBOR (TxCert era)
  ) =>
  EncCBORGroup (BabelPlutusPurpose f era)
  where
  listLen _ = 2
  listLenBound _ = 2
  encCBORGroup = \case
    BabelSpending p -> encodeWord8 0 <> encCBOR p
    BabelMinting p -> encodeWord8 1 <> encCBOR p
    BabelCertifying p -> encodeWord8 2 <> encCBOR p
    BabelRewarding p -> encodeWord8 3 <> encCBOR p
    BabelVoting p -> encodeWord8 4 <> encCBOR p
    BabelProposing p -> encodeWord8 5 <> encCBOR p
  encodedGroupSizeExpr size_ _proxy =
    encodedSizeExpr size_ (Proxy :: Proxy Word8)
      + encodedSizeExpr size_ (Proxy :: Proxy Word16)

instance
  ( forall a b. (DecCBOR a, DecCBOR b) => DecCBOR (f a b)
  , EraPParams era
  , Typeable f
  , DecCBOR (TxCert era)
  ) =>
  DecCBORGroup (BabelPlutusPurpose f era)
  where
  decCBORGroup =
    decodeWord8 >>= \case
      0 -> BabelSpending <$> decCBOR
      1 -> BabelMinting <$> decCBOR
      2 -> BabelCertifying <$> decCBOR
      3 -> BabelRewarding <$> decCBOR
      4 -> BabelVoting <$> decCBOR
      5 -> BabelProposing <$> decCBOR
      n -> fail $ "Unexpected tag for BabelPlutusPurpose: " <> show n

instance
  ( forall a b. (ToJSON a, ToJSON b) => ToJSON (f a b)
  , ToJSON (TxCert era)
  , EraPParams era
  ) =>
  ToJSON (BabelPlutusPurpose f era)
  where
  toJSON = \case
    BabelSpending n -> kindObjectWithValue "BabelSpending" n
    BabelMinting n -> kindObjectWithValue "BabelMinting" n
    BabelCertifying n -> kindObjectWithValue "BabelCertifying" n
    BabelRewarding n -> kindObjectWithValue "BabelRewarding" n
    BabelVoting n -> kindObjectWithValue "BabelVoting" n
    BabelProposing n -> kindObjectWithValue "BabelProposing" n
    where
      kindObjectWithValue name n = kindObject name ["value" .= n]

pattern VotingPurpose ::
  BabelEraScript era => f Word32 (Voter (EraCrypto era)) -> PlutusPurpose f era
pattern VotingPurpose c <- (toVotingPurpose -> Just c)
  where
    VotingPurpose c = mkVotingPurpose c

pattern ProposingPurpose ::
  BabelEraScript era => f Word32 (ProposalProcedure era) -> PlutusPurpose f era
pattern ProposingPurpose c <- (toProposingPurpose -> Just c)
  where
    ProposingPurpose c = mkProposingPurpose c
