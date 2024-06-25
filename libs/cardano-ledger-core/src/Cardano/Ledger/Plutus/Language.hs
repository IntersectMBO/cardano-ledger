{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- | This module provides data structures and operations for talking about
--     Non-native Script languages. It is expected that new languages (or new
--     versions of old languages) will be added here.
module Cardano.Ledger.Plutus.Language (
  -- * Plutus Script
  Plutus (..),
  isValidPlutus,
  PlutusBinary (..),
  PlutusRunnable (..),
  plutusFromRunnable,
  decodeWithPlutus,
  hashPlutusScript,

  -- * Value level Plutus Language version
  Language (..),
  mkLanguageEnum,
  languageToText,
  languageFromText,
  nonNativeLanguages,
  guardPlutus,

  -- * Type level Plutus Language version
  SLanguage (..),
  PlutusLanguage (..),
  plutusLanguage,
  plutusSLanguage,
  fromSLanguage,
  toSLanguage,
  withSLanguage,
  asSLanguage,
) where

import qualified Cardano.Crypto.Hash.Class as Hash (castHash, hashWith)
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  Decoder,
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  Version,
  decodeRecordNamed,
  encodeEnum,
  encodeListLen,
  getDecoderVersion,
  getVersion,
  natVersion,
  unlessDecoderVersionAtLeast,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Crypto
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Control.DeepSeq (NFData (..))
import Control.Monad (when)
import Data.Aeson (
  FromJSON (parseJSON),
  FromJSONKey (fromJSONKey),
  FromJSONKeyFunction (FromJSONKeyTextParser),
  ToJSON (toJSON),
  ToJSONKey (toJSONKey),
  Value (String),
  withText,
 )
import Data.Aeson.Types (toJSONKeyText)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64 (encode)
import Data.ByteString.Short (ShortByteString, fromShort)
import Data.Either (isRight)
import Data.Ix (Ix)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import qualified PlutusLedgerApi.Common as P (
  Data,
  EvaluationContext,
  EvaluationError,
  ExBudget,
  LogOutput,
  MajorProtocolVersion (..),
  ScriptDecodeError,
  ScriptForEvaluation,
  VerboseMode,
  serialisedScript,
 )
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import qualified PlutusLedgerApi.V4 as PV4

-- | This is a deserialized version of the `Plutus` type that can be used directly with
-- evaluation functions that rely on `evaluatePlutusRunnable`.
--
-- The only way to obtain this type is by the means of deserializing `Plutus` with
-- `decodePlutusRunnable`
newtype PlutusRunnable (l :: Language) = PlutusRunnable
  { plutusRunnable :: P.ScriptForEvaluation
  }
  deriving stock (Show, Generic)
  deriving newtype (Eq, NFData, NoThunks)

toMajorProtocolVersion :: Version -> P.MajorProtocolVersion
toMajorProtocolVersion = P.MajorProtocolVersion . getVersion

instance PlutusLanguage l => DecCBOR (PlutusRunnable l) where
  decCBOR = do
    plutus <- decCBOR
    pv <- getDecoderVersion
    either (fail . show) pure $ decodePlutusRunnable pv plutus

instance PlutusLanguage l => EncCBOR (PlutusRunnable l) where
  encCBOR = encCBOR . plutusFromRunnable

-- | Serialized representation of a Plutus script that distinguishes the language version at the
-- type level. When encoded in CBOR language version is also encoded.
newtype Plutus (l :: Language) = Plutus
  { plutusBinary :: PlutusBinary
  }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, SafeToHash, NoThunks, NFData)

plutusSLanguage :: PlutusLanguage l => proxy l -> SLanguage l
plutusSLanguage _ = isLanguage

-- | Compute a `ScriptHash` of a `Plutus` script. This function is equivalent to
-- `Cardano.Ledger.Core.hashScript`, except it is restricted to Plutus scripts
hashPlutusScript :: (PlutusLanguage l, Crypto c) => Plutus l -> ScriptHash c
hashPlutusScript plutusScript =
  ScriptHash $
    Hash.castHash $
      Hash.hashWith id (BS.singleton (plutusLanguageTag plutusScript) <> originalBytes plutusScript)

decodePlutus :: Decoder s (Language, PlutusBinary)
decodePlutus = decodeRecordNamed "Plutus" (const 2) $ (,) <$> decCBOR <*> decCBOR

-- | Plutus script decoder that will enforce the type level language equals the one
-- included in the serialization
decodeWithPlutus :: (forall si l. PlutusLanguage l => Plutus l -> Decoder si a) -> Decoder so a
decodeWithPlutus decoderAction = do
  (lang, binary) <- decodePlutus
  withSLanguage lang $ \slang ->
    decoderAction $ asSLanguage slang $ Plutus binary

instance PlutusLanguage l => DecCBOR (Plutus l) where
  decCBOR = do
    (langDecoded, binary) <- decodePlutus
    let plutus = Plutus binary
        langExpected = plutusLanguage plutus
    when (langDecoded /= langExpected) $ do
      fail $ "Expected " <> show langExpected <> ", but got: " <> show langDecoded
    pure plutus

instance PlutusLanguage l => EncCBOR (Plutus l) where
  encCBOR plutus@(Plutus binaryScript) =
    encodeListLen 2 <> encodeEnum lang <> encCBOR binaryScript
    where
      lang = plutusLanguage plutus

-- | Verify that the binary version of the Plutus script is deserializable.
isValidPlutus :: PlutusLanguage l => Version -> Plutus l -> Bool
isValidPlutus v = isRight . decodePlutusRunnable v

-- | Serialize the runnable version of the plutus script
--
-- prop> decodePlutusRunnable majVer (plutusFromRunnable pr) == Right pr
plutusFromRunnable :: PlutusRunnable l -> Plutus l
plutusFromRunnable = Plutus . PlutusBinary . P.serialisedScript . plutusRunnable

-- | Binary representation of a Plutus script.
newtype PlutusBinary = PlutusBinary {unPlutusBinary :: ShortByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (ToCBOR, FromCBOR, EncCBOR, DecCBOR, NFData, NoThunks)

instance Show PlutusBinary where
  show = show . B64.encode . fromShort . unPlutusBinary

instance DecCBOR (Annotator PlutusBinary) where
  decCBOR = pure <$> decCBOR

instance SafeToHash PlutusBinary where
  originalBytes (PlutusBinary binaryBlutus) = fromShort binaryBlutus

-- | Non-Native Plutus Script language. This is expected to be an open type. We will add
-- new Constuctors to this type as additional Plutus language versions as are added.  We
-- use an enumerated type for two reasons.
--
-- 1. We can write total functions by case analysis over the constructors
--
-- 2. We use DataKinds to make some datatypes indexed by Language.
--
-- Note that the the serialization of 'Language' depends on the ordering.
data Language
  = PlutusV1
  | PlutusV2
  | PlutusV3
  | PlutusV4
  deriving (Eq, Generic, Show, Ord, Enum, Bounded, Ix)

instance NoThunks Language

instance NFData Language

-- | Make a language from its `Enum` index.
mkLanguageEnum :: Int -> Maybe Language
mkLanguageEnum iLang
  | minLang <= iLang && iLang <= maxLang = Just $ toEnum iLang
  | otherwise = Nothing
  where
    minLang = fromEnum (minBound :: Language)
    maxLang = fromEnum (maxBound :: Language)

instance FromJSON Language where
  parseJSON = withText "Language" languageFromText

instance ToJSON Language where
  toJSON = String . languageToText

instance ToJSONKey Language where
  toJSONKey = toJSONKeyText languageToText

instance FromJSONKey Language where
  fromJSONKey = FromJSONKeyTextParser languageFromText

languageToText :: Language -> Text
languageToText PlutusV1 = "PlutusV1"
languageToText PlutusV2 = "PlutusV2"
languageToText PlutusV3 = "PlutusV3"
languageToText PlutusV4 = "PlutusV4"

languageFromText :: MonadFail m => Text -> m Language
languageFromText "PlutusV1" = pure PlutusV1
languageFromText "PlutusV2" = pure PlutusV2
languageFromText "PlutusV3" = pure PlutusV3
languageFromText "PlutusV4" = pure PlutusV4
languageFromText lang = fail $ "Error decoding Language: " ++ show lang

instance ToCBOR Language where
  toCBOR = Plain.encodeEnum

instance FromCBOR Language where
  fromCBOR = Plain.decodeEnumBounded

instance EncCBOR Language

instance DecCBOR Language

nonNativeLanguages :: [Language]
nonNativeLanguages = [minBound .. maxBound]

-- | Singleton for '@Language@'
data SLanguage (l :: Language) where
  SPlutusV1 :: SLanguage 'PlutusV1
  SPlutusV2 :: SLanguage 'PlutusV2
  SPlutusV3 :: SLanguage 'PlutusV3
  SPlutusV4 :: SLanguage 'PlutusV4

deriving instance Eq (SLanguage l)

deriving instance Show (SLanguage l)

instance PlutusLanguage l => ToCBOR (SLanguage l) where
  toCBOR = toCBOR . fromSLanguage

instance PlutusLanguage l => FromCBOR (SLanguage l) where
  fromCBOR = toSLanguage =<< fromCBOR @Language

instance PlutusLanguage l => EncCBOR (SLanguage l)

instance PlutusLanguage l => DecCBOR (SLanguage l)

-- | Reflection for '@SLanguage@'
fromSLanguage :: PlutusLanguage l => SLanguage l -> Language
fromSLanguage = plutusLanguage
{-# DEPRECATED fromSLanguage "In favor of `plutusLanguage`" #-}

-- | Construct value level laguage version from the type level
plutusLanguage :: forall l proxy. PlutusLanguage l => proxy l -> Language
plutusLanguage _ = case isLanguage @l of
  SPlutusV1 -> PlutusV1
  SPlutusV2 -> PlutusV2
  SPlutusV3 -> PlutusV3
  SPlutusV4 -> PlutusV4

-- | For implicit reflection on '@SLanguage@'
-- See "Cardano.Ledger.Alonzo.Plutus.TxInfo" for example usage
class Typeable l => PlutusLanguage (l :: Language) where
  isLanguage :: SLanguage l

  -- | Tag that will be used as a prefix to compute the `ScriptHash`
  plutusLanguageTag :: Plutus l -> Word8

  decodePlutusRunnable ::
    -- | Which major protocol version to use for deserialization and further execution
    Version ->
    -- | Binary version of the script that will be deserialized
    Plutus l ->
    Either P.ScriptDecodeError (PlutusRunnable l)

  evaluatePlutusRunnable ::
    -- | Which major protocol version to use for script execution
    Version ->
    -- | Whether to produce log output
    P.VerboseMode ->
    -- | Includes the cost model to use for tallying up the execution costs
    P.EvaluationContext ->
    -- | The resource budget which must not be exceeded during evaluation
    P.ExBudget ->
    -- | The script to evaluate
    PlutusRunnable l ->
    -- | The arguments to the script
    [P.Data] ->
    (P.LogOutput, Either P.EvaluationError P.ExBudget)

  -- | Similar to `evaluatePlutusRunnable`, except does not require `P.ExBudget` to be
  -- provided as input and instead computes it as output. This function is meant to be
  -- used for testing.
  evaluatePlutusRunnableBudget ::
    -- | Which major protocol version to use for script execution
    Version ->
    -- | Whether to produce log output
    P.VerboseMode ->
    -- | Includes the cost model to use for tallying up the execution costs
    P.EvaluationContext ->
    -- | The script to evaluate
    PlutusRunnable l ->
    -- | The arguments to the script
    [P.Data] ->
    (P.LogOutput, Either P.EvaluationError P.ExBudget)

instance PlutusLanguage 'PlutusV1 where
  isLanguage = SPlutusV1
  plutusLanguageTag _ = 0x01
  decodePlutusRunnable pv (Plutus (PlutusBinary bs)) =
    PlutusRunnable <$> PV1.deserialiseScript (toMajorProtocolVersion pv) bs
  evaluatePlutusRunnable pv vm ec exBudget (PlutusRunnable rs) =
    PV1.evaluateScriptRestricting (toMajorProtocolVersion pv) vm ec exBudget rs
  evaluatePlutusRunnableBudget pv vm ec (PlutusRunnable rs) =
    PV1.evaluateScriptCounting (toMajorProtocolVersion pv) vm ec rs

instance PlutusLanguage 'PlutusV2 where
  isLanguage = SPlutusV2
  plutusLanguageTag _ = 0x02
  decodePlutusRunnable pv (Plutus (PlutusBinary bs)) =
    PlutusRunnable <$> PV2.deserialiseScript (toMajorProtocolVersion pv) bs
  evaluatePlutusRunnable pv vm ec exBudget (PlutusRunnable rs) =
    PV2.evaluateScriptRestricting (toMajorProtocolVersion pv) vm ec exBudget rs
  evaluatePlutusRunnableBudget pv vm ec (PlutusRunnable rs) =
    PV2.evaluateScriptCounting (toMajorProtocolVersion pv) vm ec rs

instance PlutusLanguage 'PlutusV3 where
  isLanguage = SPlutusV3
  plutusLanguageTag _ = 0x03
  decodePlutusRunnable pv (Plutus (PlutusBinary bs)) =
    PlutusRunnable <$> PV3.deserialiseScript (toMajorProtocolVersion pv) bs
  evaluatePlutusRunnable pv vm ec exBudget (PlutusRunnable rs) ds =
    PV3.evaluateScriptRestricting (toMajorProtocolVersion pv) vm ec exBudget rs (head ds)
  evaluatePlutusRunnableBudget pv vm ec (PlutusRunnable rs) ds =
    PV3.evaluateScriptCounting (toMajorProtocolVersion pv) vm ec rs (head ds)

instance PlutusLanguage 'PlutusV4 where
  isLanguage = SPlutusV4
  plutusLanguageTag _ = 0x04
  decodePlutusRunnable pv (Plutus (PlutusBinary bs)) =
    PlutusRunnable <$> PV4.deserialiseScript (toMajorProtocolVersion pv) bs
  evaluatePlutusRunnable pv vm ec exBudget (PlutusRunnable rs) ds =
    PV4.evaluateScriptRestricting (toMajorProtocolVersion pv) vm ec exBudget rs (head ds)
  evaluatePlutusRunnableBudget pv vm ec (PlutusRunnable rs) ds =
    PV4.evaluateScriptCounting (toMajorProtocolVersion pv) vm ec rs (head ds)

toSLanguage :: forall l m. (PlutusLanguage l, MonadFail m) => Language -> m (SLanguage l)
toSLanguage lang
  | fromSLanguage thisLanguage == lang = pure thisLanguage
  | otherwise =
      fail $
        "Plutus language mismatch. Expected "
          ++ show thisLanguage
          ++ ", but got: "
          ++ show lang
  where
    thisLanguage :: SLanguage l
    thisLanguage = isLanguage

asSLanguage :: SLanguage l -> proxy l -> proxy l
asSLanguage = flip const

withSLanguage :: Language -> (forall l. PlutusLanguage l => SLanguage l -> a) -> a
withSLanguage l f =
  case l of
    PlutusV1 -> f SPlutusV1
    PlutusV2 -> f SPlutusV2
    PlutusV3 -> f SPlutusV3
    PlutusV4 -> f SPlutusV4

-- | Prevent decoding a version of Plutus until
-- the appropriate protocol version.
guardPlutus :: Language -> Decoder s ()
guardPlutus lang =
  let v = case lang of
        PlutusV1 -> natVersion @5
        PlutusV2 -> natVersion @7
        PlutusV3 -> natVersion @9
        PlutusV4 -> natVersion @11
   in unlessDecoderVersionAtLeast v $
        fail (show lang <> " is not supported until " <> show v <> " major protocol version")
