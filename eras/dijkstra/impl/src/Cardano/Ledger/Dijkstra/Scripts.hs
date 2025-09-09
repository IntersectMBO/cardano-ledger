{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Scripts (
  PlutusScript (..),
  DijkstraPlutusPurpose (..),
  DijkstraEraScript (..),
  DijkstraNativeScript (MkDijkstraNativeScript),
  DijkstraNativeScriptRaw (..),
  pattern GuardingPurpose,
  evalDijkstraNativeScript,
) where

import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (..),
  AsItem,
  AsIx (..),
  AsIxItem,
  alonzoScriptPrefixTag,
 )
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  Annotator,
  CBORGroup (..),
  DecCBOR (decCBOR),
  DecCBORGroup (..),
  EncCBOR (encCBOR),
  EncCBORGroup (..),
  ToCBOR (..),
  decodeWord8,
  encodeWord8,
  encodedSizeExpr,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  Wrapped (..),
  decode,
  encode,
  (!>),
  (<!),
  (<*!),
 )
import Cardano.Ledger.Conway.Governance (ProposalProcedure, Voter)
import Cardano.Ledger.Conway.Scripts
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams ()
import Cardano.Ledger.Dijkstra.TxCert ()
import Cardano.Ledger.Mary.Value (PolicyID)
import Cardano.Ledger.MemoBytes
import Cardano.Ledger.Plutus (Language (..), Plutus, SLanguage (..), plutusSLanguage)
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData (..), rwhnf)
import Data.Aeson (KeyValue (..), ToJSON (..))
import Data.MemPack (MemPack (..), packTagM, packedTagByteCount, unknownTagM, unpackTagM)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict as Seq (StrictSeq (Empty, (:<|)))
import qualified Data.Sequence.Strict as SSeq
import Data.Set as Set (Set, member)
import Data.Typeable (Proxy (..), Typeable)
import Data.Word (Word16, Word32, Word8)
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

instance Inject (ConwayPlutusPurpose f era) (DijkstraPlutusPurpose f era) where
  inject = \case
    ConwaySpending p -> DijkstraSpending p
    ConwayMinting p -> DijkstraMinting p
    ConwayCertifying p -> DijkstraCertifying p
    ConwayRewarding p -> DijkstraRewarding p
    ConwayVoting p -> DijkstraVoting p
    ConwayProposing p -> DijkstraProposing p

deriving via
  CBORGroup (DijkstraPlutusPurpose f era)
  instance
    ( Typeable f
    , EraPParams era
    , forall a b. (DecCBOR a, DecCBOR b) => DecCBOR (f a b)
    , forall a b. (EncCBOR a, EncCBOR b) => EncCBOR (f a b)
    , EraTxCert era
    ) =>
    DecCBOR (DijkstraPlutusPurpose f era)

deriving via
  CBORGroup (DijkstraPlutusPurpose f era)
  instance
    ( Typeable f
    , EraPParams era
    , forall a b. (EncCBOR a, EncCBOR b) => EncCBOR (f a b)
    , EraTxCert era
    ) =>
    EncCBOR (DijkstraPlutusPurpose f era)

instance
  ( Typeable f
  , EraPParams era
  , forall a b. (DecCBOR a, DecCBOR b) => DecCBOR (f a b)
  , DecCBOR (TxCert era)
  ) =>
  DecCBORGroup (DijkstraPlutusPurpose f era)
  where
  decCBORGroup =
    decodeWord8 >>= \case
      0 -> DijkstraSpending <$> decCBOR
      1 -> DijkstraMinting <$> decCBOR
      2 -> DijkstraCertifying <$> decCBOR
      3 -> DijkstraRewarding <$> decCBOR
      4 -> DijkstraVoting <$> decCBOR
      5 -> DijkstraProposing <$> decCBOR
      6 -> DijkstraGuarding <$> decCBOR
      n -> fail $ "Unexpected tag for DijkstraPlutusPurpose: " <> show n

instance
  ( Typeable f
  , EraPParams era
  , forall a b. (EncCBOR a, EncCBOR b) => EncCBOR (f a b)
  , EncCBOR (TxCert era)
  ) =>
  EncCBORGroup (DijkstraPlutusPurpose f era)
  where
  listLen _ = 2
  listLenBound _ = 2
  encCBORGroup = \case
    DijkstraSpending p -> encodeWord8 0 <> encCBOR p
    DijkstraMinting p -> encodeWord8 1 <> encCBOR p
    DijkstraCertifying p -> encodeWord8 2 <> encCBOR p
    DijkstraRewarding p -> encodeWord8 3 <> encCBOR p
    DijkstraVoting p -> encodeWord8 4 <> encCBOR p
    DijkstraProposing p -> encodeWord8 5 <> encCBOR p
    DijkstraGuarding p -> encodeWord8 6 <> encCBOR p
  encodedGroupSizeExpr size_ _proxy =
    encodedSizeExpr size_ (Proxy @Word8) + encodedSizeExpr size_ (Proxy @Word16)

instance
  ( forall a b. (ToJSON a, ToJSON b) => ToJSON (f a b)
  , ToJSON (TxCert era)
  , EraPParams era
  ) =>
  ToJSON (DijkstraPlutusPurpose f era)
  where
  toJSON = \case
    DijkstraSpending n -> kindObjectWithValue "DijkstraSpending" n
    DijkstraMinting n -> kindObjectWithValue "DijkstraMinting" n
    DijkstraCertifying n -> kindObjectWithValue "DijkstraCertifying" n
    DijkstraRewarding n -> kindObjectWithValue "DijkstraRewarding" n
    DijkstraVoting n -> kindObjectWithValue "DijkstraVoting" n
    DijkstraProposing n -> kindObjectWithValue "DijkstraProposing" n
    DijkstraGuarding n -> kindObjectWithValue "DijkstraGuarding" n
    where
      kindObjectWithValue name n = kindObject name ["value" .= n]

deriving instance (EraTxCert era, EraPParams era) => Eq (DijkstraPlutusPurpose AsItem era)

deriving instance (EraTxCert era, EraPParams era) => Eq (DijkstraPlutusPurpose AsIx era)

deriving instance (EraTxCert era, EraPParams era) => Eq (DijkstraPlutusPurpose AsIxItem era)

instance (EraPParams era, NFData (TxCert era)) => NFData (DijkstraPlutusPurpose AsItem era)

instance (EraPParams era, NFData (TxCert era)) => NFData (DijkstraPlutusPurpose AsIx era)

instance (EraPParams era, NFData (TxCert era)) => NFData (DijkstraPlutusPurpose AsIxItem era)

instance (EraPParams era, NoThunks (TxCert era)) => NoThunks (DijkstraPlutusPurpose AsItem era)

instance (EraPParams era, NoThunks (TxCert era)) => NoThunks (DijkstraPlutusPurpose AsIx era)

instance (EraPParams era, NoThunks (TxCert era)) => NoThunks (DijkstraPlutusPurpose AsIxItem era)

deriving instance (EraPParams era, EraTxCert era) => Ord (DijkstraPlutusPurpose AsItem era)

deriving instance (EraPParams era, EraTxCert era) => Ord (DijkstraPlutusPurpose AsIx era)

deriving instance (EraPParams era, EraTxCert era) => Ord (DijkstraPlutusPurpose AsIxItem era)

deriving instance (EraPParams era, EraTxCert era) => Show (DijkstraPlutusPurpose AsItem era)

deriving instance (EraPParams era, EraTxCert era) => Show (DijkstraPlutusPurpose AsIx era)

deriving instance (EraPParams era, EraTxCert era) => Show (DijkstraPlutusPurpose AsIxItem era)

data DijkstraNativeScriptRaw era
  = DijkstraRequireSignature !(KeyHash 'Witness)
  | DijkstraRequireAllOf !(StrictSeq (DijkstraNativeScript era))
  | DijkstraRequireAnyOf !(StrictSeq (DijkstraNativeScript era))
  | DijkstraRequireMOf !Int !(StrictSeq (DijkstraNativeScript era))
  | DijkstraTimeStart !SlotNo
  | DijkstraTimeExpire !SlotNo
  deriving (Eq, Generic, NFData)

deriving instance Show (DijkstraNativeScriptRaw era)

deriving instance Era era => NoThunks (DijkstraNativeScriptRaw era)

instance Era era => EncCBOR (DijkstraNativeScriptRaw era) where
  encCBOR =
    encode . \case
      DijkstraRequireSignature hash -> Sum DijkstraRequireSignature 0 !> To hash
      DijkstraRequireAllOf xs -> Sum DijkstraRequireAllOf 1 !> To xs
      DijkstraRequireAnyOf xs -> Sum DijkstraRequireAnyOf 2 !> To xs
      DijkstraRequireMOf m xs -> Sum DijkstraRequireMOf 3 !> To m !> To xs
      DijkstraTimeStart m -> Sum DijkstraTimeStart 4 !> To m
      DijkstraTimeExpire m -> Sum DijkstraTimeExpire 5 !> To m

instance Era era => DecCBOR (Annotator (DijkstraNativeScriptRaw era)) where
  decCBOR = decode (Summands "DijkstraNativeScriptRaw" decRaw)
    where
      decRaw :: Word -> Decode 'Open (Annotator (DijkstraNativeScriptRaw era))
      decRaw 0 = Ann (SumD DijkstraRequireSignature <! From)
      decRaw 1 = Ann (SumD DijkstraRequireAllOf) <*! D (sequence <$> decCBOR)
      decRaw 2 = Ann (SumD DijkstraRequireAnyOf) <*! D (sequence <$> decCBOR)
      decRaw 3 = Ann (SumD DijkstraRequireMOf) <*! Ann From <*! D (sequence <$> decCBOR)
      decRaw 4 = Ann (SumD DijkstraTimeStart <! From)
      decRaw 5 = Ann (SumD DijkstraTimeExpire <! From)
      decRaw n = Invalid n

newtype DijkstraNativeScript era = MkDijkstraNativeScript (MemoBytes (DijkstraNativeScriptRaw era))
  deriving (Eq, Generic)
  deriving newtype (ToCBOR, NFData, SafeToHash)

deriving instance Show (DijkstraNativeScript era)

instance Era era => NoThunks (DijkstraNativeScript era)

instance Era era => EncCBOR (DijkstraNativeScript era)

instance Era era => DecCBOR (Annotator (DijkstraNativeScript era)) where
  decCBOR = fmap MkDijkstraNativeScript <$> decCBOR

instance Memoized (DijkstraNativeScript era) where
  type RawType (DijkstraNativeScript era) = DijkstraNativeScriptRaw era

instance EqRaw (DijkstraNativeScript era) where
  eqRaw = eqRawDijkstraNativeScript
    where
      eqRawDijkstraNativeScript dns1 dns2 = go (getMemoRawType dns1) (getMemoRawType dns2)
      seqEq Empty Empty = True
      seqEq (x :<| xs) (y :<| ys) = eqRawDijkstraNativeScript x y && seqEq xs ys
      seqEq _ _ = False
      go (DijkstraRequireSignature kh1) (DijkstraRequireSignature kh2) = kh1 == kh2
      go (DijkstraRequireAllOf ts1) (DijkstraRequireAllOf ts2) = seqEq ts1 ts2
      go (DijkstraRequireAnyOf ts1) (DijkstraRequireAnyOf ts2) = seqEq ts1 ts2
      go (DijkstraRequireMOf n1 ts1) (DijkstraRequireMOf n2 ts2) = n1 == n2 && seqEq ts1 ts2
      go (DijkstraTimeStart sn1) (DijkstraTimeStart sn2) = sn1 == sn2
      go (DijkstraTimeExpire sn1) (DijkstraTimeExpire sn2) = sn1 == sn2
      go _ _ = False

instance Era era => MemPack (DijkstraNativeScript era) where
  packedByteCount (MkDijkstraNativeScript mb) = byteCountMemoBytes mb
  packM (MkDijkstraNativeScript mb) = packMemoBytesM mb
  unpackM = MkDijkstraNativeScript <$> unpackMemoBytesM (eraProtVerLow @era)

instance EraScript DijkstraEra where
  type Script DijkstraEra = AlonzoScript DijkstraEra
  type NativeScript DijkstraEra = Timelock DijkstraEra

  upgradeScript = \case
    NativeScript ts -> NativeScript $ translateTimelock ts
    PlutusScript (ConwayPlutusV1 s) -> PlutusScript $ DijkstraPlutusV1 s
    PlutusScript (ConwayPlutusV2 s) -> PlutusScript $ DijkstraPlutusV2 s
    PlutusScript (ConwayPlutusV3 s) -> PlutusScript $ DijkstraPlutusV3 s

  scriptPrefixTag = alonzoScriptPrefixTag

  getNativeScript (NativeScript ts) = Just ts
  getNativeScript _ = Nothing

  fromNativeScript = NativeScript

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

  type PlutusPurpose f DijkstraEra = DijkstraPlutusPurpose f DijkstraEra

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
    DijkstraSpending x -> DijkstraSpending $ f x
    DijkstraMinting x -> DijkstraMinting $ f x
    DijkstraCertifying x -> DijkstraCertifying $ f x
    DijkstraRewarding x -> DijkstraRewarding $ f x
    DijkstraVoting x -> DijkstraVoting $ f x
    DijkstraProposing x -> DijkstraProposing $ f x
    DijkstraGuarding x -> DijkstraGuarding $ f x

  mkSpendingPurpose = DijkstraSpending

  toSpendingPurpose (DijkstraSpending i) = Just i
  toSpendingPurpose _ = Nothing

  mkMintingPurpose = DijkstraMinting

  toMintingPurpose (DijkstraMinting i) = Just i
  toMintingPurpose _ = Nothing

  mkCertifyingPurpose = DijkstraCertifying

  toCertifyingPurpose (DijkstraCertifying i) = Just i
  toCertifyingPurpose _ = Nothing

  mkRewardingPurpose = DijkstraRewarding

  toRewardingPurpose (DijkstraRewarding i) = Just i
  toRewardingPurpose _ = Nothing

  upgradePlutusPurposeAsIx = \case
    ConwaySpending (AsIx ix) -> DijkstraSpending (AsIx ix)
    ConwayMinting (AsIx ix) -> DijkstraMinting (AsIx ix)
    ConwayCertifying (AsIx ix) -> DijkstraCertifying (AsIx ix)
    ConwayRewarding (AsIx ix) -> DijkstraRewarding (AsIx ix)
    ConwayVoting (AsIx ix) -> DijkstraVoting (AsIx ix)
    ConwayProposing (AsIx ix) -> DijkstraProposing (AsIx ix)

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

instance ConwayEraScript DijkstraEra where
  mkVotingPurpose = DijkstraVoting

  toVotingPurpose (DijkstraVoting i) = Just i
  toVotingPurpose _ = Nothing

  mkProposingPurpose = DijkstraProposing

  toProposingPurpose (DijkstraProposing i) = Just i
  toProposingPurpose _ = Nothing

class DijkstraEraScript era where
  mkGuardingPurpose :: f Word32 ScriptHash -> PlutusPurpose f era
  toGuardingPurpose :: PlutusPurpose f era -> Maybe (f Word32 ScriptHash)

instance DijkstraEraScript DijkstraEra where
  mkGuardingPurpose = DijkstraGuarding

  toGuardingPurpose (DijkstraGuarding i) = Just i
  toGuardingPurpose _ = Nothing

pattern GuardingPurpose ::
  DijkstraEraScript era => f Word32 ScriptHash -> PlutusPurpose f era
pattern GuardingPurpose c <- (toGuardingPurpose -> Just c)
  where
    GuardingPurpose c = mkGuardingPurpose c

evalDijkstraNativeScript ::
  (AllegraEraScript era, NativeScript era ~ DijkstraNativeScript era) =>
  Set.Set (KeyHash 'Witness) ->
  ValidityInterval ->
  NativeScript era ->
  Bool
evalDijkstraNativeScript keyHashes (ValidityInterval txStart txExp) = go
  where
    -- the evaluation will stop as soon as it reaches the required number of valid scripts
    isValidMOf n SSeq.Empty = n <= 0
    isValidMOf n (ts SSeq.:<| tss) =
      n <= 0 || if go ts then isValidMOf (n - 1) tss else isValidMOf n tss
    go = \case
      RequireTimeStart lockStart -> lockStart `lteNegInfty` txStart
      RequireTimeExpire lockExp -> txExp `ltePosInfty` lockExp
      RequireSignature hash -> hash `Set.member` keyHashes
      RequireAllOf xs -> all go xs
      RequireAnyOf xs -> any go xs
      RequireMOf m xs -> isValidMOf m xs
      _ -> error "Impossible: All NativeScripts should have been accounted for"
