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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Scripts (
  Tag (..),
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

  -- * Re-exports
  module Cardano.Ledger.Plutus.CostModels,
  module Cardano.Ledger.Plutus.ExUnits,
)
where

import Cardano.Ledger.Allegra.Scripts (Timelock, eqTimelockRaw, translateTimelock)
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (decCBOR),
  Decoder,
  DecoderError (..),
  EncCBOR (encCBOR),
  ToCBOR (toCBOR),
  Version,
  cborError,
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
import Cardano.Ledger.Crypto (Crypto)
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
import Control.DeepSeq (NFData (..), deepseq, rwhnf)
import Control.Monad (guard)
import Data.Aeson (ToJSON (..), Value (String))
import Data.Kind (Type)
import Data.Maybe (fromJust, isJust)
import Data.Typeable
import Data.Word (Word8)
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
  ) =>
  AlonzoEraScript era
  where
  data PlutusScript era :: Type

  eraMaxLanguage :: Language

  toPlutusScript :: Script era -> Maybe (PlutusScript era)
  default toPlutusScript :: Script era ~ AlonzoScript era => Script era -> Maybe (PlutusScript era)
  toPlutusScript = \case
    PlutusScript ps -> Just ps
    _ -> Nothing

  fromPlutusScript :: PlutusScript era -> Script era
  default fromPlutusScript :: Script era ~ AlonzoScript era => PlutusScript era -> Script era
  fromPlutusScript = PlutusScript

  -- | Returns Nothing, whenver plutus language is not supported for this era.
  mkPlutusScript :: PlutusLanguage l => Plutus l -> Maybe (PlutusScript era)

  withPlutusScript ::
    PlutusScript era ->
    (forall l. PlutusLanguage l => Plutus l -> a) ->
    a

-- | Marker indicating the part of a transaction for which this script is acting
-- as a validator.
data Tag
  = -- | Validates spending a script-locked UTxO
    Spend
  | -- | Validates minting new tokens
    Mint
  | -- | Validates certificate transactions
    Cert
  | -- | Validates withdrawal from a reward account
    Rewrd
  deriving (Eq, Generic, Ord, Show, Enum, Bounded)

instance NoThunks Tag

instance NFData Tag where
  rnf = rwhnf

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

plutusScriptBinary :: AlonzoEraScript era => PlutusScript era -> PlutusBinary
plutusScriptBinary ps = withPlutusScript ps plutusBinary

-- | Verifies whether Plutus script is well formed or not, which simply means whether it
-- deserializes successfully or not.
isValidPlutusScript :: AlonzoEraScript era => Version -> PlutusScript era -> Bool
isValidPlutusScript pv ps = withPlutusScript ps (isValidPlutus pv)

-- =======================================================

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

  eraMaxLanguage = PlutusV1

  mkPlutusScript plutus =
    case plutusSLanguage plutus of
      SPlutusV1 -> Just $ AlonzoPlutusV1 plutus
      _ -> Nothing

  withPlutusScript (AlonzoPlutusV1 plutus) f = f plutus

instance Eq (PlutusScript era) => EqRaw (AlonzoScript era) where
  eqRaw = eqAlonzoScriptRaw

instance AlonzoEraScript era => ToJSON (AlonzoScript era) where
  toJSON = String . serializeAsHexText

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

tagToWord8 :: Tag -> Word8
tagToWord8 = toEnum . fromEnum

word8ToTag :: Word8 -> Maybe Tag
word8ToTag e
  | fromEnum e > fromEnum (Prelude.maxBound :: Tag) = Nothing
  | fromEnum e < fromEnum (minBound :: Tag) = Nothing
  | otherwise = Just $ toEnum (fromEnum e)

instance EncCBOR Tag where
  encCBOR = encCBOR . tagToWord8

instance DecCBOR Tag where
  decCBOR =
    word8ToTag <$> decCBOR >>= \case
      Nothing -> cborError $ DecoderErrorCustom "Tag" "Unknown redeemer tag"
      Just n -> pure n
  {-# INLINE decCBOR #-}

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
