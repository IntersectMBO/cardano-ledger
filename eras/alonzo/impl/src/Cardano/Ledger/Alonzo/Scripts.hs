{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Scripts (
  PlutusBinary (..),
  AlonzoScript (NativeScript, PlutusScript),
  Script,
  isPlutusScript,
  validScript,
  eqAlonzoScriptRaw,
  AlonzoEraScript (..),
  eraLanguages,
  eraUnsupportedLanguage,
  PlutusScript (..),
  withPlutusScriptLanguage,
  plutusScriptLanguage,
  decodePlutusScript,
  plutusScriptBinary,
  mkBinaryPlutusScript,
  isValidPlutusScript,
  toPlutusSLanguage,
  alonzoScriptPrefixTag,
  lookupPlutusScript,

  -- ** Plutus Purpose
  pattern SpendingPurpose,
  pattern MintingPurpose,
  pattern CertifyingPurpose,
  pattern RewardingPurpose,
  AlonzoPlutusPurpose (..),
  AsItem (..),
  AsIx (..),
  AsIxItem (..),
  toAsItem,
  toAsIx,

  -- * Re-exports
  module Cardano.Ledger.Plutus.CostModels,
  module Cardano.Ledger.Plutus.ExUnits,
) where

import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.TxCert ()
import Cardano.Ledger.BaseTypes (ProtVer (..), kindObject)
import Cardano.Ledger.Binary (
  Annotator,
  CBORGroup (..),
  DecCBOR (decCBOR),
  DecCBORGroup (..),
  Decoder,
  EncCBOR (..),
  EncCBORGroup (..),
  ToCBOR (toCBOR),
  Version,
  decodeWord8,
  encodeWord8,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (Ann, D, From, Invalid, SumD, Summands),
  Encode (Sum, To),
  Wrapped (..),
  decode,
  encode,
  (!>),
  (<*!),
 )
import Cardano.Ledger.Binary.Plain (serializeAsHexText)
import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Value (PolicyID)
import Cardano.Ledger.MemoBytes (EqRaw (..))
import Cardano.Ledger.Plutus.CostModels
import Cardano.Ledger.Plutus.ExUnits
import Cardano.Ledger.Plutus.Language (
  Language (..),
  Plutus (..),
  PlutusBinary (..),
  PlutusLanguage (..),
  SLanguage (..),
  asSLanguage,
  isValidPlutus,
  plutusLanguage,
  plutusSLanguage,
  withSLanguage,
 )
import Cardano.Ledger.Shelley.Scripts (ShelleyEraScript (..), nativeMultiSigTag)
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad (guard, (>=>))
import Data.Aeson (ToJSON (..), Value (String), object, (.=))
import qualified Data.ByteString as BS
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import Data.MemPack
import Data.Typeable
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.Stack
import NoThunks.Class (NoThunks (..))

class
  ( EraScript era
  , Eq (PlutusScript era)
  , Ord (PlutusScript era)
  , Show (PlutusScript era)
  , NoThunks (PlutusScript era)
  , NFData (PlutusScript era)
  , SafeToHash (PlutusScript era)
  , Eq (PlutusPurpose AsItem era)
  , Show (PlutusPurpose AsItem era)
  , EncCBOR (PlutusPurpose AsItem era)
  , DecCBOR (PlutusPurpose AsItem era)
  , NoThunks (PlutusPurpose AsItem era)
  , NFData (PlutusPurpose AsItem era)
  , Eq (PlutusPurpose AsIx era)
  , Ord (PlutusPurpose AsIx era)
  , Show (PlutusPurpose AsIx era)
  , EncCBOR (PlutusPurpose AsIx era)
  , DecCBOR (PlutusPurpose AsIx era)
  , EncCBORGroup (PlutusPurpose AsIx era)
  , DecCBORGroup (PlutusPurpose AsIx era)
  , NoThunks (PlutusPurpose AsIx era)
  , NFData (PlutusPurpose AsIx era)
  , Eq (PlutusPurpose AsIxItem era)
  , Show (PlutusPurpose AsIxItem era)
  , NoThunks (PlutusPurpose AsIxItem era)
  , NFData (PlutusPurpose AsIxItem era)
  , AllegraEraScript era
  ) =>
  AlonzoEraScript era
  where
  data PlutusScript era :: Type

  type PlutusPurpose (f :: Type -> Type -> Type) era = (r :: Type) | r -> era

  -- | Highest supported Plutus language version for this era.
  eraMaxLanguage :: Language

  -- | Attempt to extract a `PlutusScript` from a wrapper type family `Script`. Whenevr
  -- `Script` is a native script `Nothing` will be returned
  toPlutusScript :: Script era -> Maybe (PlutusScript era)
  default toPlutusScript :: Script era ~ AlonzoScript era => Script era -> Maybe (PlutusScript era)
  toPlutusScript = \case
    PlutusScript ps -> Just ps
    _ -> Nothing

  -- | Convert a `PlutusScript` to a wrapper type family `Script`
  fromPlutusScript :: PlutusScript era -> Script era
  default fromPlutusScript :: Script era ~ AlonzoScript era => PlutusScript era -> Script era
  fromPlutusScript = PlutusScript

  -- | Returns Nothing, whenver plutus language is not supported for this era.
  mkPlutusScript :: (PlutusLanguage l, MonadFail m) => Plutus l -> m (PlutusScript era)

  -- | Give a `PlutusScript` apply a function that can handle `Plutus` scripts of all
  -- known versions.
  withPlutusScript ::
    PlutusScript era ->
    (forall l. PlutusLanguage l => Plutus l -> a) ->
    a

  hoistPlutusPurpose ::
    (forall ix it. g ix it -> f ix it) ->
    PlutusPurpose g era ->
    PlutusPurpose f era

  mkSpendingPurpose :: f Word32 TxIn -> PlutusPurpose f era

  toSpendingPurpose :: PlutusPurpose f era -> Maybe (f Word32 TxIn)

  mkMintingPurpose :: f Word32 PolicyID -> PlutusPurpose f era

  toMintingPurpose :: PlutusPurpose f era -> Maybe (f Word32 PolicyID)

  mkCertifyingPurpose :: f Word32 (TxCert era) -> PlutusPurpose f era

  toCertifyingPurpose :: PlutusPurpose f era -> Maybe (f Word32 (TxCert era))

  mkRewardingPurpose :: f Word32 RewardAccount -> PlutusPurpose f era

  toRewardingPurpose :: PlutusPurpose f era -> Maybe (f Word32 RewardAccount)

  upgradePlutusPurposeAsIx ::
    AlonzoEraScript (PreviousEra era) =>
    PlutusPurpose AsIx (PreviousEra era) ->
    PlutusPurpose AsIx era

mkBinaryPlutusScript ::
  (MonadFail m, AlonzoEraScript era) =>
  Language ->
  PlutusBinary ->
  m (PlutusScript era)
mkBinaryPlutusScript lang pb = withSLanguage lang (mkPlutusScript . (`asSLanguage` Plutus pb))

-- | Apply a function to a plutus script, but only if it is of expected language version,
-- otherwise it will return Nothing.
withPlutusScriptLanguage ::
  AlonzoEraScript era =>
  Language ->
  PlutusScript era ->
  (forall l. PlutusLanguage l => Plutus l -> a) ->
  Maybe a
withPlutusScriptLanguage lang ps f =
  withPlutusScript ps $ \plutus ->
    f plutus <$ guard (plutusLanguage plutus == lang)

-- | Attempt to extract the version aware `Plutus` script, but only if it matches the
-- language version supplied. This is useful whenever the version is known by some other
-- means.
toPlutusSLanguage ::
  forall l era.
  (PlutusLanguage l, AlonzoEraScript era) =>
  SLanguage l ->
  PlutusScript era ->
  Maybe (Plutus l)
toPlutusSLanguage _ ps = withPlutusScript ps gcast

-- | Get value level plutus language of the plutus script
plutusScriptLanguage :: AlonzoEraScript era => PlutusScript era -> Language
plutusScriptLanguage ps = withPlutusScript ps plutusLanguage

-- | Extract binary representation of the script.
plutusScriptBinary :: AlonzoEraScript era => PlutusScript era -> PlutusBinary
plutusScriptBinary ps = withPlutusScript ps plutusBinary

-- | Verifies whether Plutus script is well formed or not, which simply means whether it
-- deserializes successfully or not.
isValidPlutusScript :: AlonzoEraScript era => Version -> PlutusScript era -> Bool
isValidPlutusScript pv ps = withPlutusScript ps (isValidPlutus pv)

-- Alonzo Plutus Purpose =======================================================

newtype AsIx ix it = AsIx {unAsIx :: ix}
  deriving stock (Show)
  deriving newtype (Eq, Ord, NFData, NoThunks, EncCBOR, DecCBOR, Generic)

newtype AsItem ix it = AsItem {unAsItem :: it}
  deriving stock (Show)
  deriving newtype (Eq, Ord, NFData, NoThunks, EncCBOR, DecCBOR, Generic)

data AsIxItem ix it = AsIxItem
  { asIndex :: !ix
  , asItem :: !it
  }
  deriving (Eq, Ord, Show, Generic)

instance (NoThunks ix, NoThunks it) => NoThunks (AsIxItem ix it)

instance (NFData ix, NFData it) => NFData (AsIxItem ix it) where
  rnf (AsIxItem ix it) = ix `deepseq` rnf it

instance ToJSON ix => ToJSON (AsIx ix it) where
  toJSON (AsIx i) = object ["index" .= toJSON i]

instance ToJSON it => ToJSON (AsItem ix it) where
  toJSON (AsItem i) = object ["item" .= toJSON i]

instance (ToJSON ix, ToJSON it) => ToJSON (AsIxItem ix it) where
  toJSON (AsIxItem ix it) =
    object
      [ "index" .= toJSON ix
      , "item" .= toJSON it
      ]

toAsItem :: AsIxItem ix it -> AsItem ix it
toAsItem (AsIxItem _ it) = AsItem it

toAsIx :: AsIxItem ix it -> AsIx ix it
toAsIx (AsIxItem ix _) = AsIx ix

data AlonzoPlutusPurpose f era
  = AlonzoSpending !(f Word32 TxIn)
  | AlonzoMinting !(f Word32 PolicyID)
  | AlonzoCertifying !(f Word32 (TxCert era))
  | AlonzoRewarding !(f Word32 RewardAccount)
  deriving (Generic)

deriving instance Eq (AlonzoPlutusPurpose AsIx era)

deriving instance Ord (AlonzoPlutusPurpose AsIx era)

deriving instance Show (AlonzoPlutusPurpose AsIx era)

instance NoThunks (AlonzoPlutusPurpose AsIx era)

deriving instance Eq (TxCert era) => Eq (AlonzoPlutusPurpose AsItem era)

deriving instance Show (TxCert era) => Show (AlonzoPlutusPurpose AsItem era)

instance NoThunks (TxCert era) => NoThunks (AlonzoPlutusPurpose AsItem era)

deriving instance Eq (TxCert era) => Eq (AlonzoPlutusPurpose AsIxItem era)

deriving instance Show (TxCert era) => Show (AlonzoPlutusPurpose AsIxItem era)

instance NoThunks (TxCert era) => NoThunks (AlonzoPlutusPurpose AsIxItem era)

instance
  (forall a b. (NFData a, NFData b) => NFData (f a b), NFData (TxCert era), Era era) =>
  NFData (AlonzoPlutusPurpose f era)
  where
  rnf = \case
    AlonzoSpending x -> rnf x
    AlonzoMinting x -> rnf x
    AlonzoCertifying x -> rnf x
    AlonzoRewarding x -> rnf x

instance
  ( forall a b. (EncCBOR a, EncCBOR b) => EncCBOR (f a b)
  , Era era
  , EncCBOR (TxCert era)
  ) =>
  EncCBORGroup (AlonzoPlutusPurpose f era)
  where
  listLen _ = 2
  listLenBound _ = 2
  encCBORGroup = \case
    AlonzoSpending p -> encodeWord8 0 <> encCBOR p
    AlonzoMinting p -> encodeWord8 1 <> encCBOR p
    AlonzoCertifying p -> encodeWord8 2 <> encCBOR p
    AlonzoRewarding p -> encodeWord8 3 <> encCBOR p

instance
  ( forall a b. (DecCBOR a, DecCBOR b) => DecCBOR (f a b)
  , Era era
  , Typeable f
  , DecCBOR (TxCert era)
  ) =>
  DecCBORGroup (AlonzoPlutusPurpose f era)
  where
  decCBORGroup =
    decodeWord8 >>= \case
      0 -> AlonzoSpending <$> decCBOR
      1 -> AlonzoMinting <$> decCBOR
      2 -> AlonzoCertifying <$> decCBOR
      3 -> AlonzoRewarding <$> decCBOR
      n -> fail $ "Unexpected tag for AlonzoPlutusPurpose: " <> show n

deriving via
  (CBORGroup (AlonzoPlutusPurpose f era))
  instance
    ( forall a b. (EncCBOR a, EncCBOR b) => EncCBOR (f a b)
    , Era era
    , EncCBOR (TxCert era)
    ) =>
    EncCBOR (AlonzoPlutusPurpose f era)

deriving via
  (CBORGroup (AlonzoPlutusPurpose f era))
  instance
    ( forall a b. (EncCBOR a, EncCBOR b) => EncCBOR (f a b)
    , forall a b. (DecCBOR a, DecCBOR b) => DecCBOR (f a b)
    , Era era
    , Typeable f
    , EncCBOR (TxCert era)
    , DecCBOR (TxCert era)
    ) =>
    DecCBOR (AlonzoPlutusPurpose f era)

instance
  ( forall a b. (ToJSON a, ToJSON b) => ToJSON (f a b)
  , ToJSON (TxCert era)
  , Era era
  ) =>
  ToJSON (AlonzoPlutusPurpose f era)
  where
  toJSON = \case
    AlonzoSpending n -> kindObjectWithValue "AlonzoSpending" n
    AlonzoMinting n -> kindObjectWithValue "AlonzoMinting" n
    AlonzoCertifying n -> kindObjectWithValue "AlonzoCertifying" n
    AlonzoRewarding n -> kindObjectWithValue "AlonzoRewarding" n
    where
      kindObjectWithValue name n = kindObject name ["value" .= n]

pattern SpendingPurpose ::
  AlonzoEraScript era => f Word32 TxIn -> PlutusPurpose f era
pattern SpendingPurpose c <- (toSpendingPurpose -> Just c)
  where
    SpendingPurpose c = mkSpendingPurpose c

pattern MintingPurpose ::
  AlonzoEraScript era => f Word32 PolicyID -> PlutusPurpose f era
pattern MintingPurpose c <- (toMintingPurpose -> Just c)
  where
    MintingPurpose c = mkMintingPurpose c

pattern CertifyingPurpose ::
  AlonzoEraScript era => f Word32 (TxCert era) -> PlutusPurpose f era
pattern CertifyingPurpose c <- (toCertifyingPurpose -> Just c)
  where
    CertifyingPurpose c = mkCertifyingPurpose c

pattern RewardingPurpose ::
  AlonzoEraScript era => f Word32 RewardAccount -> PlutusPurpose f era
pattern RewardingPurpose c <- (toRewardingPurpose -> Just c)
  where
    RewardingPurpose c = mkRewardingPurpose c

-- Alonzo Script ===============================================================

-- | Scripts in the Alonzo Era, Either a native script or a Plutus script.
data AlonzoScript era
  = NativeScript !(NativeScript era)
  | PlutusScript !(PlutusScript era)
  deriving (Generic)

instance
  ( Era era
  , MemPack (PlutusScript era)
  , MemPack (NativeScript era)
  ) =>
  MemPack (AlonzoScript era)
  where
  packedByteCount = \case
    NativeScript script -> packedTagByteCount + packedByteCount script
    PlutusScript script -> packedTagByteCount + packedByteCount script
  packM = \case
    NativeScript script -> packTagM 0 >> packM script
    PlutusScript script -> packTagM 1 >> packM script
  {-# INLINE packM #-}
  unpackM =
    unpackTagM >>= \case
      0 -> NativeScript <$> unpackM
      1 -> PlutusScript <$> unpackM
      n -> unknownTagM @(AlonzoScript era) n
  {-# INLINE unpackM #-}

deriving instance (Eq (PlutusScript era), Eq (NativeScript era)) => Eq (AlonzoScript era)

instance
  (Era era, NoThunks (PlutusScript era), NoThunks (NativeScript era)) =>
  NoThunks (AlonzoScript era)

instance (NFData (PlutusScript era), NFData (NativeScript era)) => NFData (AlonzoScript era) where
  rnf = \case
    NativeScript ts -> rnf ts
    PlutusScript ps -> rnf ps

instance (AlonzoEraScript era, Script era ~ AlonzoScript era) => Show (AlonzoScript era) where
  show (NativeScript x) = "NativeScript " ++ show x
  show s@(PlutusScript plutus) =
    "PlutusScript " ++ show (plutusScriptLanguage plutus) ++ " " ++ show (hashScript @era s)

-- | Both constructors know their original bytes
instance (SafeToHash (PlutusScript era), SafeToHash (NativeScript era)) => SafeToHash (AlonzoScript era) where
  originalBytes (NativeScript t) = originalBytes t
  originalBytes (PlutusScript plutus) = originalBytes plutus

isPlutusScript :: AlonzoEraScript era => Script era -> Bool
isPlutusScript = isJust . toPlutusScript

instance EraScript AlonzoEra where
  type Script AlonzoEra = AlonzoScript AlonzoEra
  type NativeScript AlonzoEra = Timelock AlonzoEra

  upgradeScript = NativeScript . translateTimelock

  scriptPrefixTag = alonzoScriptPrefixTag

  getNativeScript = \case
    NativeScript ts -> Just ts
    _ -> Nothing

  fromNativeScript = NativeScript

alonzoScriptPrefixTag ::
  (AlonzoEraScript era, AlonzoScript era ~ Script era) =>
  Script era ->
  BS.ByteString
alonzoScriptPrefixTag = \case
  NativeScript _ -> nativeMultiSigTag -- "\x00"
  PlutusScript plutusScript -> BS.singleton (withPlutusScript plutusScript plutusLanguageTag)

instance ShelleyEraScript AlonzoEra where
  mkRequireSignature = mkRequireSignatureTimelock
  getRequireSignature = getRequireSignatureTimelock

  mkRequireAllOf = mkRequireAllOfTimelock
  getRequireAllOf = getRequireAllOfTimelock

  mkRequireAnyOf = mkRequireAnyOfTimelock
  getRequireAnyOf = getRequireAnyOfTimelock

  mkRequireMOf = mkRequireMOfTimelock
  getRequireMOf = getRequireMOfTimelock

instance AllegraEraScript AlonzoEra where
  mkTimeStart = mkTimeStartTimelock
  getTimeStart = getTimeStartTimelock

  mkTimeExpire = mkTimeExpireTimelock
  getTimeExpire = getTimeExpireTimelock

instance AlonzoEraScript AlonzoEra where
  newtype PlutusScript AlonzoEra = AlonzoPlutusV1 (Plutus 'PlutusV1)
    deriving newtype (Eq, Ord, Show, NFData, NoThunks, SafeToHash, Generic)

  type PlutusPurpose f AlonzoEra = AlonzoPlutusPurpose f AlonzoEra

  eraMaxLanguage = PlutusV1

  mkPlutusScript plutus =
    case plutusSLanguage plutus of
      SPlutusV1 -> pure $ AlonzoPlutusV1 plutus
      slang -> eraUnsupportedLanguage @AlonzoEra slang

  withPlutusScript (AlonzoPlutusV1 plutus) f = f plutus

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

  upgradePlutusPurposeAsIx =
    error "Impossible: No `PlutusScript` and `AlonzoEraScript` instances in the previous era"

instance
  ( Eq (PlutusScript era)
  , EqRaw (NativeScript era)
  ) =>
  EqRaw (AlonzoScript era)
  where
  eqRaw = eqAlonzoScriptRaw

instance AlonzoEraScript era => ToJSON (AlonzoScript era) where
  toJSON = String . serializeAsHexText

-- | It might seem that this instance unnecessarily utilizes a zero Tag, but it is needed for
-- forward compatibility with plutus scripts from future eras.
--
-- That being said, currently this instance is not used at all, since reference scripts where
-- introduced in Babbage era and `MemPack` for now is only used for `TxOut`s
instance MemPack (PlutusScript AlonzoEra) where
  packedByteCount = \case
    AlonzoPlutusV1 script -> packedTagByteCount + packedByteCount script
  packM = \case
    AlonzoPlutusV1 script -> packTagM 0 >> packM script
  {-# INLINE packM #-}
  unpackM =
    unpackTagM >>= \case
      0 -> AlonzoPlutusV1 <$> unpackM
      n -> unknownTagM @(PlutusScript AlonzoEra) n
  {-# INLINE unpackM #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

decodePlutusScript ::
  forall era l s.
  (AlonzoEraScript era, PlutusLanguage l) =>
  SLanguage l ->
  Decoder s (PlutusScript era)
decodePlutusScript slang = do
  pb <- decCBOR
  mkPlutusScript $ asSLanguage slang $ Plutus pb

instance AlonzoEraScript era => EncCBOR (AlonzoScript era)

instance AlonzoEraScript era => ToCBOR (AlonzoScript era) where
  toCBOR = toEraCBOR @era . encode . encodeScript

encodeScript :: AlonzoEraScript era => AlonzoScript era -> Encode 'Open (AlonzoScript era)
encodeScript = \case
  NativeScript i -> Sum NativeScript 0 !> To i
  PlutusScript plutusScript -> withPlutusScript plutusScript $ \plutus@(Plutus pb) ->
    case plutusSLanguage plutus of
      SPlutusV1 -> Sum (PlutusScript . fromJust . mkPlutusScript . Plutus @'PlutusV1) 1 !> To pb
      SPlutusV2 -> Sum (PlutusScript . fromJust . mkPlutusScript . Plutus @'PlutusV2) 2 !> To pb
      SPlutusV3 -> Sum (PlutusScript . fromJust . mkPlutusScript . Plutus @'PlutusV3) 3 !> To pb
      SPlutusV4 -> Sum (PlutusScript . fromJust . mkPlutusScript . Plutus @'PlutusV4) 4 !> To pb

instance AlonzoEraScript era => DecCBOR (Annotator (AlonzoScript era)) where
  decCBOR = decode (Summands "AlonzoScript" decodeScript)
    where
      decodeAnnPlutus slang =
        Ann (SumD PlutusScript) <*! Ann (D (decodePlutusScript slang))
      {-# INLINE decodeAnnPlutus #-}
      decodeScript :: Word -> Decode 'Open (Annotator (AlonzoScript era))
      decodeScript = \case
        0 -> Ann (SumD NativeScript) <*! From
        1 -> decodeAnnPlutus SPlutusV1
        2 -> decodeAnnPlutus SPlutusV2
        3 -> decodeAnnPlutus SPlutusV3
        n -> Invalid n
      {-# INLINE decodeScript #-}
  {-# INLINE decCBOR #-}

-- | Verify that every `Script` represents a valid script. Force native scripts to Normal
-- Form, to ensure that there are no bottoms and deserialize `Plutus` scripts into a
-- `Cardano.Ledger.Plutus.Language.PlutusRunnable`.
validScript :: (HasCallStack, AlonzoEraScript era) => ProtVer -> Script era -> Bool
validScript pv script =
  case toPlutusScript script of
    Just plutusScript -> isValidPlutusScript (pvMajor pv) plutusScript
    Nothing ->
      case getNativeScript script of
        Just timelockScript -> deepseq timelockScript True
        Nothing -> error "Impossible: There are only Native and Plutus scripts available"

-- | Check the equality of two underlying types, while ignoring their binary
-- representation, which `Eq` instance normally does. This is used for testing.
eqAlonzoScriptRaw ::
  ( Eq (PlutusScript era)
  , EqRaw (NativeScript era)
  ) =>
  AlonzoScript era -> AlonzoScript era -> Bool
eqAlonzoScriptRaw (NativeScript t1) (NativeScript t2) = eqRaw t1 t2
eqAlonzoScriptRaw (PlutusScript ps1) (PlutusScript ps2) = ps1 == ps2
eqAlonzoScriptRaw _ _ = False

eraLanguages :: forall era. AlonzoEraScript era => [Language]
eraLanguages = [minBound .. eraMaxLanguage @era]

eraUnsupportedLanguage ::
  forall era l m proxy a. (Era era, PlutusLanguage l, MonadFail m) => proxy l -> m a
eraUnsupportedLanguage slang =
  fail $ show (plutusLanguage slang) <> " isn't supported in the " <> eraName @era <> " era"

-- | Having a Map with scripts and a script hash, lookup the plutus script. Returns
-- Nothing when script is missing or it is not a PlutusScript
lookupPlutusScript ::
  AlonzoEraScript era =>
  ScriptHash ->
  Map.Map ScriptHash (Script era) ->
  Maybe (PlutusScript era)
lookupPlutusScript scriptHash = Map.lookup scriptHash >=> toPlutusScript
