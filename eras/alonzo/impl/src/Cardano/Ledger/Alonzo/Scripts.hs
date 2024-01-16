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
{-# LANGUAGE NamedFieldPuns #-}
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
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Scripts (
  PlutusBinary (..),
  AlonzoScript (TimelockScript, PlutusScript),
  Script,
  isPlutusScript,
  validScript,
  eqAlonzoScriptRaw,
  AlonzoEraScript (..),
  PlutusScript (..),
  withPlutusScriptLanguage,
  plutusScriptLanguage,
  decodePlutusScript,
  plutusScriptBinary,
  mkBinaryPlutusScript,
  isValidPlutusScript,
  toPlutusSLanguage,

  -- ** Plutus Purpose
  AlonzoPlutusPurpose (..),
  AsItem (..),
  AsIndex (..),

  -- * Re-exports
  module Cardano.Ledger.Plutus.CostModels,
  module Cardano.Ledger.Plutus.ExUnits,
)
where

import Cardano.Ledger.Address (RewardAcnt)
import Cardano.Ledger.Allegra.Scripts (Timelock, eqTimelockRaw, translateTimelock)
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.TxCert ()
import Cardano.Ledger.BaseTypes (ProtVer (..), kindObject)
import Cardano.Ledger.Binary (
  Annotator,
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
  (<!),
  (<*!),
 )
import Cardano.Ledger.Binary.Plain (serializeAsHexText)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
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
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.Scripts (nativeMultiSigTag)
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad (guard)
import Data.Aeson (ToJSON (..), Value (String), object, (.=))
import Data.Kind (Type)
import Data.Maybe (fromJust, isJust)
import Data.Typeable
import Data.Word (Word16, Word32, Word8)
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
  , Eq (PlutusPurpose AsIndex era)
  , Ord (PlutusPurpose AsIndex era)
  , Show (PlutusPurpose AsIndex era)
  , EncCBOR (PlutusPurpose AsIndex era)
  , DecCBOR (PlutusPurpose AsIndex era)
  , EncCBORGroup (PlutusPurpose AsIndex era)
  , DecCBORGroup (PlutusPurpose AsIndex era)
  , NoThunks (PlutusPurpose AsIndex era)
  , NFData (PlutusPurpose AsIndex era)
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
  mkPlutusScript :: PlutusLanguage l => Plutus l -> Maybe (PlutusScript era)

  -- | Give a `PlutusScript` apply a function that can handle `Plutus` scripts of all
  -- known versions.
  withPlutusScript ::
    PlutusScript era ->
    (forall l. PlutusLanguage l => Plutus l -> a) ->
    a

  plutusPurposeSpendingTxIn :: PlutusPurpose AsItem era -> Maybe (TxIn (EraCrypto era))

  upgradePlutusPurposeAsIndex ::
    AlonzoEraScript (PreviousEra era) =>
    PlutusPurpose AsIndex (PreviousEra era) ->
    PlutusPurpose AsIndex era

mkBinaryPlutusScript :: AlonzoEraScript era => Language -> PlutusBinary -> Maybe (PlutusScript era)
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

newtype AsIndex ix it = AsIndex {unAsIndex :: ix}
  deriving newtype (Eq, Ord, Show, NFData, NoThunks, EncCBOR, DecCBOR, Generic)

newtype AsItem ix it = AsItem {unAsItem :: it}
  deriving newtype (Eq, Ord, Show, NFData, NoThunks, EncCBOR, DecCBOR, Generic)

instance ToJSON a => ToJSON (AsIndex a b) where
  toJSON (AsIndex i) = object ["index" .= toJSON i]

instance ToJSON b => ToJSON (AsItem a b) where
  toJSON (AsItem i) = object ["item" .= toJSON i]

data AlonzoPlutusPurpose f era
  = AlonzoSpending !(f Word32 (TxIn (EraCrypto era)))
  | AlonzoMinting !(f Word32 (PolicyID (EraCrypto era)))
  | AlonzoCertifying !(f Word32 (TxCert era))
  | AlonzoRewarding !(f Word32 (RewardAcnt (EraCrypto era)))
  deriving (Generic)

deriving instance Eq (AlonzoPlutusPurpose AsIndex era)
deriving instance Ord (AlonzoPlutusPurpose AsIndex era)
deriving instance Show (AlonzoPlutusPurpose AsIndex era)
instance NoThunks (AlonzoPlutusPurpose AsIndex era)

deriving instance Eq (TxCert era) => Eq (AlonzoPlutusPurpose AsItem era)
deriving instance Show (TxCert era) => Show (AlonzoPlutusPurpose AsItem era)
instance NoThunks (TxCert era) => NoThunks (AlonzoPlutusPurpose AsItem era)

instance
  (forall a b. (NFData a, NFData b) => NFData (f a b), NFData (TxCert era), Era era) =>
  NFData (AlonzoPlutusPurpose f era)
  where
  rnf = \case
    AlonzoSpending x -> rnf x
    AlonzoMinting x -> rnf x
    AlonzoCertifying x -> rnf x
    AlonzoRewarding x -> rnf x

instance Era era => EncCBORGroup (AlonzoPlutusPurpose AsIndex era) where
  listLen _ = 2
  listLenBound _ = 2
  encCBORGroup = \case
    AlonzoSpending (AsIndex redeemerIx) -> encodeWord8 0 <> encCBOR redeemerIx
    AlonzoMinting (AsIndex redeemerIx) -> encodeWord8 1 <> encCBOR redeemerIx
    AlonzoCertifying (AsIndex redeemerIx) -> encodeWord8 2 <> encCBOR redeemerIx
    AlonzoRewarding (AsIndex redeemerIx) -> encodeWord8 3 <> encCBOR redeemerIx
  encodedGroupSizeExpr size_ _proxy =
    encodedSizeExpr size_ (Proxy :: Proxy Word8)
      + encodedSizeExpr size_ (Proxy :: Proxy Word16)

instance Era era => DecCBORGroup (AlonzoPlutusPurpose AsIndex era) where
  decCBORGroup =
    decodeWord8 >>= \case
      0 -> AlonzoSpending . AsIndex <$> decCBOR
      1 -> AlonzoMinting . AsIndex <$> decCBOR
      2 -> AlonzoCertifying . AsIndex <$> decCBOR
      3 -> AlonzoRewarding . AsIndex <$> decCBOR
      n -> fail $ "Unexpected tag for AlonzoPlutusPurpose: " <> show n

-- | Incorrect CBOR implementation. Missing length encoding. Must keep it for backwards
-- compatibility
instance Era era => EncCBOR (AlonzoPlutusPurpose AsIndex era) where
  encCBOR = encCBORGroup

-- | Incorrect CBOR implementation. Missing length encoding. Must keep it for backwards
-- compatibility
instance Era era => DecCBOR (AlonzoPlutusPurpose AsIndex era) where
  decCBOR = decCBORGroup

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

-- | /Note/ - serialization of `AlonzoPlutusPurpose` `AsItem`
--
-- * Tags do not match the `AlonzoPlutusPurpose` `AsIndex`. Unfortunate inconsistency
--
-- * It is only used for predicate failures. Thus we can change it after Conway to be
--   consistent with `AlonzoPlutusPurpose` `AsIndex`
instance (Era era, EncCBOR (TxCert era)) => EncCBOR (AlonzoPlutusPurpose AsItem era) where
  encCBOR = \case
    AlonzoSpending (AsItem x) -> encode (Sum (AlonzoSpending @_ @era . AsItem) 1 !> To x)
    AlonzoMinting (AsItem x) -> encode (Sum (AlonzoMinting @_ @era . AsItem) 0 !> To x)
    AlonzoCertifying (AsItem x) -> encode (Sum (AlonzoCertifying . AsItem) 3 !> To x)
    AlonzoRewarding (AsItem x) -> encode (Sum (AlonzoRewarding @_ @era . AsItem) 2 !> To x)

-- | See note on the `EncCBOR` instace.
instance (Era era, DecCBOR (TxCert era)) => DecCBOR (AlonzoPlutusPurpose AsItem era) where
  decCBOR = decode (Summands "AlonzoPlutusPurpose" dec)
    where
      dec 1 = SumD (AlonzoSpending . AsItem) <! From
      dec 0 = SumD (AlonzoMinting . AsItem) <! From
      dec 3 = SumD (AlonzoCertifying . AsItem) <! From
      dec 2 = SumD (AlonzoRewarding . AsItem) <! From
      dec n = Invalid n

-- Alonzo Script ===============================================================

-- | Scripts in the Alonzo Era, Either a Timelock script or a Plutus script.
data AlonzoScript era
  = TimelockScript !(Timelock era)
  | PlutusScript !(PlutusScript era)
  deriving (Generic)

deriving instance Eq (PlutusScript era) => Eq (AlonzoScript era)

instance (Era era, NoThunks (PlutusScript era)) => NoThunks (AlonzoScript era)

instance NFData (PlutusScript era) => NFData (AlonzoScript era) where
  rnf = \case
    TimelockScript ts -> rnf ts
    PlutusScript ps -> rnf ps

instance (AlonzoEraScript era, Script era ~ AlonzoScript era) => Show (AlonzoScript era) where
  show (TimelockScript x) = "TimelockScript " ++ show x
  show s@(PlutusScript plutus) =
    "PlutusScript " ++ show (plutusScriptLanguage plutus) ++ " " ++ show (hashScript @era s)

-- | Both constructors know their original bytes
instance SafeToHash (PlutusScript era) => SafeToHash (AlonzoScript era) where
  originalBytes (TimelockScript t) = originalBytes t
  originalBytes (PlutusScript plutus) = originalBytes plutus

isPlutusScript :: AlonzoEraScript era => Script era -> Bool
isPlutusScript = isJust . toPlutusScript

instance Crypto c => EraScript (AlonzoEra c) where
  type Script (AlonzoEra c) = AlonzoScript (AlonzoEra c)
  type NativeScript (AlonzoEra c) = Timelock (AlonzoEra c)

  upgradeScript = TimelockScript . translateTimelock

  scriptPrefixTag = \case
    TimelockScript _ -> nativeMultiSigTag -- "\x00"
    PlutusScript (AlonzoPlutusV1 _) -> "\x01"

  getNativeScript = \case
    TimelockScript ts -> Just ts
    _ -> Nothing

  fromNativeScript = TimelockScript

instance Crypto c => AlonzoEraScript (AlonzoEra c) where
  newtype PlutusScript (AlonzoEra c) = AlonzoPlutusV1 (Plutus 'PlutusV1)
    deriving newtype (Eq, Ord, Show, NFData, NoThunks, SafeToHash, Generic)

  type PlutusPurpose f (AlonzoEra c) = AlonzoPlutusPurpose f (AlonzoEra c)

  eraMaxLanguage = PlutusV1

  mkPlutusScript plutus =
    case plutusSLanguage plutus of
      SPlutusV1 -> Just $ AlonzoPlutusV1 plutus
      _ -> Nothing

  withPlutusScript (AlonzoPlutusV1 plutus) f = f plutus

  plutusPurposeSpendingTxIn = \case
    AlonzoSpending (AsItem txIn) -> Just txIn
    _ -> Nothing

  upgradePlutusPurposeAsIndex =
    error "Impossible: No `PlutusScript` and `AlonzoEraScript` instances in the previous era"

instance Eq (PlutusScript era) => EqRaw (AlonzoScript era) where
  eqRaw = eqAlonzoScriptRaw

instance AlonzoEraScript era => ToJSON (AlonzoScript era) where
  toJSON = String . serializeAsHexText

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
  case mkPlutusScript $ asSLanguage slang $ Plutus pb of
    Nothing ->
      fail $ show (plutusLanguage slang) ++ " is not supported in " ++ eraName @era ++ " era."
    Just plutusScript -> pure plutusScript

instance AlonzoEraScript era => EncCBOR (AlonzoScript era)

instance AlonzoEraScript era => ToCBOR (AlonzoScript era) where
  toCBOR = toEraCBOR @era . encode . encodeScript

encodeScript :: AlonzoEraScript era => AlonzoScript era -> Encode 'Open (AlonzoScript era)
encodeScript = \case
  TimelockScript i -> Sum TimelockScript 0 !> To i
  PlutusScript plutusScript -> withPlutusScript plutusScript $ \plutus@(Plutus pb) ->
    case plutusSLanguage plutus of
      SPlutusV1 -> Sum (PlutusScript . fromJust . mkPlutusScript . Plutus @'PlutusV1) 1 !> To pb
      SPlutusV2 -> Sum (PlutusScript . fromJust . mkPlutusScript . Plutus @'PlutusV2) 2 !> To pb
      SPlutusV3 -> Sum (PlutusScript . fromJust . mkPlutusScript . Plutus @'PlutusV3) 3 !> To pb

instance AlonzoEraScript era => DecCBOR (Annotator (AlonzoScript era)) where
  decCBOR = decode (Summands "AlonzoScript" decodeScript)
    where
      decodeAnnPlutus slang =
        Ann (SumD PlutusScript) <*! Ann (D (decodePlutusScript slang))
      {-# INLINE decodeAnnPlutus #-}
      decodeScript :: Word -> Decode 'Open (Annotator (AlonzoScript era))
      decodeScript = \case
        0 -> Ann (SumD TimelockScript) <*! From
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
eqAlonzoScriptRaw :: Eq (PlutusScript era) => AlonzoScript era -> AlonzoScript era -> Bool
eqAlonzoScriptRaw (TimelockScript t1) (TimelockScript t2) = eqTimelockRaw t1 t2
eqAlonzoScriptRaw (PlutusScript ps1) (PlutusScript ps2) = ps1 == ps2
eqAlonzoScriptRaw _ _ = False
