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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
  PlutusArgs (..),
  plutusLanguage,
  plutusSLanguage,
  fromSLanguage,
  toSLanguage,
  withSLanguage,
  asSLanguage,
  withSamePlutusLanguage,

  -- * Plutus Script Context
  LegacyPlutusArgs (..),
  PlutusScriptContext,
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
  decodeScriptContextFromData,
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
import Control.DeepSeq (NFData (..), deepseq)
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
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Typeable (Typeable, gcast)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import PlutusCore (DefaultFun, DefaultUni)
import qualified PlutusLedgerApi.Common as P (
  Data,
  EvaluationContext,
  EvaluationError,
  ExBudget,
  LogOutput,
  MajorProtocolVersion (..),
  PlutusLedgerLanguage (PlutusV1, PlutusV2, PlutusV3),
  ScriptDecodeError,
  ScriptForEvaluation,
  VerboseMode,
  mkTermToEvaluate,
  serialisedScript,
 )
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import Prettyprinter (Doc, Pretty (..), align, indent, line, vsep, (<+>))
import System.Random.Stateful (Random, Uniform (..), UniformRange (..), uniformEnumM, uniformEnumRM)
import qualified UntypedPlutusCore as UPLC

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
hashPlutusScript :: forall c l. (Crypto c, PlutusLanguage l) => Plutus l -> ScriptHash c
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
  deriving (Eq, Generic, Show, Ord, Enum, Bounded, Ix)

instance NoThunks Language

instance NFData Language

instance Random Language

instance Uniform Language where
  uniformM = uniformEnumM

instance UniformRange Language where
  uniformRM = uniformEnumRM

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

languageFromText :: MonadFail m => Text -> m Language
languageFromText "PlutusV1" = pure PlutusV1
languageFromText "PlutusV2" = pure PlutusV2
languageFromText "PlutusV3" = pure PlutusV3
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

type family PlutusScriptContext (l :: Language) = r | r -> l where
  PlutusScriptContext 'PlutusV1 = PV1.ScriptContext
  PlutusScriptContext 'PlutusV2 = PV2.ScriptContext
  PlutusScriptContext 'PlutusV3 = PV3.ScriptContext

data LegacyPlutusArgs l
  = -- | Scripts that require 2 arguments.
    LegacyPlutusArgs2
      -- | Redeemer
      !P.Data
      -- | PlutusScriptContext
      !(PlutusScriptContext l)
  | -- | Scripts that require 3 arguments. Which is only PlutusV1/V2 spending scripts
    LegacyPlutusArgs3
      -- | Mandatory Spending Datum
      !P.Data
      -- | Redeemer
      !P.Data
      -- | PlutusScriptContext
      !(PlutusScriptContext l)

deriving instance Eq (PlutusScriptContext l) => Eq (LegacyPlutusArgs l)
deriving instance Show (PlutusScriptContext l) => Show (LegacyPlutusArgs l)

instance NFData (PlutusScriptContext l) => NFData (LegacyPlutusArgs l) where
  rnf = \case
    LegacyPlutusArgs2 redeemer scriptContext -> redeemer `deepseq` rnf scriptContext
    LegacyPlutusArgs3 datum redeemer scriptContext -> datum `deepseq` redeemer `deepseq` rnf scriptContext

-- TODO: Change NFData instances to not go through Data and move to Plutus repo
instance NFData PV1.ScriptContext where
  rnf = rnf . PV3.toData

instance NFData PV2.ScriptContext where
  rnf = rnf . PV3.toData

instance NFData PV3.ScriptContext where
  rnf = rnf . PV3.toData

instance (PlutusLanguage l, PV3.ToData (PlutusScriptContext l)) => EncCBOR (LegacyPlutusArgs l) where
  encCBOR = encCBOR . legacyPlutusArgsToData

instance (PlutusLanguage l, PV3.FromData (PlutusScriptContext l)) => DecCBOR (LegacyPlutusArgs l) where
  decCBOR =
    decCBOR >>= \case
      [redeemer, scriptContextData] ->
        LegacyPlutusArgs2 redeemer <$> decodeScriptContextFromData scriptContextData
      [datum, redeemer, scriptContextData] ->
        LegacyPlutusArgs3 datum redeemer <$> decodeScriptContextFromData scriptContextData
      args ->
        fail $ "Invalid number of aruments " <> show (length args) <> " is encoded for " <> show lang
    where
      lang = plutusLanguage (Proxy @l)

instance Pretty (PlutusScriptContext l) => Pretty (LegacyPlutusArgs l) where
  pretty = \case
    LegacyPlutusArgs2 redeemer scriptContext ->
      let argsList =
            [ "Redeemer:"
            , indent i (pretty redeemer)
            , "ScriptContext:"
            , indent i (pretty scriptContext)
            ]
       in argsHeader 2 argsList
    LegacyPlutusArgs3 datum redeemer scriptContext ->
      let argsList =
            [ "Datum:"
            , indent i (pretty datum)
            , "Redeemer:"
            , indent i (pretty redeemer)
            , "ScriptContext:"
            , indent i (pretty scriptContext)
            ]
       in argsHeader 3 argsList
    where
      argsHeader :: Int -> [Doc ann] -> Doc ann
      argsHeader n argsList =
        "LegacyPlutusArgs" <+> pretty n <+> ":" <+> line <+> "  " <+> align (vsep argsList)
      i = 5

legacyPlutusArgsToData :: PV3.ToData (PlutusScriptContext l) => LegacyPlutusArgs l -> [P.Data]
legacyPlutusArgsToData = \case
  LegacyPlutusArgs2 redeemer scriptContext -> [redeemer, PV3.toData scriptContext]
  LegacyPlutusArgs3 datum redeemer scriptContext -> [datum, redeemer, PV3.toData scriptContext]

-- | For implicit reflection on '@SLanguage@'
-- See "Cardano.Ledger.Alonzo.Plutus.TxInfo" for example usage
class
  ( Typeable l
  , NFData (PlutusArgs l)
  , EncCBOR (PlutusArgs l)
  , DecCBOR (PlutusArgs l)
  , Pretty (PlutusArgs l)
  , Show (PlutusArgs l)
  , Eq (PlutusArgs l)
  ) =>
  PlutusLanguage (l :: Language)
  where
  data PlutusArgs l :: Type

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
    PlutusArgs l ->
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
    PlutusArgs l ->
    (P.LogOutput, Either P.EvaluationError P.ExBudget)

  mkTermToEvaluate ::
    -- | Which major protocol version to use for script execution
    Version ->
    -- | The script to evaluate
    PlutusRunnable l ->
    -- | The arguments to the script
    PlutusArgs l ->
    Either P.EvaluationError (UPLC.Term UPLC.NamedDeBruijn DefaultUni DefaultFun ())

instance PlutusLanguage 'PlutusV1 where
  newtype PlutusArgs 'PlutusV1 = PlutusV1Args {unPlutusV1Args :: LegacyPlutusArgs 'PlutusV1}
    deriving newtype (Eq, Show, Pretty, EncCBOR, DecCBOR, NFData)
  isLanguage = SPlutusV1
  plutusLanguageTag _ = 0x01
  decodePlutusRunnable pv (Plutus (PlutusBinary bs)) =
    PlutusRunnable <$> PV1.deserialiseScript (toMajorProtocolVersion pv) bs
  evaluatePlutusRunnable pv vm ec exBudget (PlutusRunnable rs) =
    PV1.evaluateScriptRestricting (toMajorProtocolVersion pv) vm ec exBudget rs
      . legacyPlutusArgsToData
      . unPlutusV1Args
  evaluatePlutusRunnableBudget pv vm ec (PlutusRunnable rs) =
    PV1.evaluateScriptCounting (toMajorProtocolVersion pv) vm ec rs
      . legacyPlutusArgsToData
      . unPlutusV1Args
  mkTermToEvaluate pv (PlutusRunnable rs) =
    P.mkTermToEvaluate P.PlutusV1 (toMajorProtocolVersion pv) rs
      . legacyPlutusArgsToData
      . unPlutusV1Args

instance PlutusLanguage 'PlutusV2 where
  newtype PlutusArgs 'PlutusV2 = PlutusV2Args {unPlutusV2Args :: LegacyPlutusArgs 'PlutusV2}
    deriving newtype (Eq, Show, Pretty, EncCBOR, DecCBOR, NFData)
  isLanguage = SPlutusV2
  plutusLanguageTag _ = 0x02
  decodePlutusRunnable pv (Plutus (PlutusBinary bs)) =
    PlutusRunnable <$> PV2.deserialiseScript (toMajorProtocolVersion pv) bs
  evaluatePlutusRunnable pv vm ec exBudget (PlutusRunnable rs) =
    PV2.evaluateScriptRestricting (toMajorProtocolVersion pv) vm ec exBudget rs
      . legacyPlutusArgsToData
      . unPlutusV2Args
  evaluatePlutusRunnableBudget pv vm ec (PlutusRunnable rs) =
    PV2.evaluateScriptCounting (toMajorProtocolVersion pv) vm ec rs
      . legacyPlutusArgsToData
      . unPlutusV2Args
  mkTermToEvaluate pv (PlutusRunnable rs) =
    P.mkTermToEvaluate P.PlutusV2 (toMajorProtocolVersion pv) rs
      . legacyPlutusArgsToData
      . unPlutusV2Args

instance PlutusLanguage 'PlutusV3 where
  newtype PlutusArgs 'PlutusV3 = PlutusV3Args {unPlutusV3Args :: PV3.ScriptContext}
    deriving newtype (Eq, Show, Pretty, EncCBOR, DecCBOR, NFData)
  isLanguage = SPlutusV3
  plutusLanguageTag _ = 0x03
  decodePlutusRunnable pv (Plutus (PlutusBinary bs)) =
    PlutusRunnable <$> PV3.deserialiseScript (toMajorProtocolVersion pv) bs
  evaluatePlutusRunnable pv vm ec exBudget (PlutusRunnable rs) =
    PV3.evaluateScriptRestricting (toMajorProtocolVersion pv) vm ec exBudget rs
      . PV3.toData
      . unPlutusV3Args
  evaluatePlutusRunnableBudget pv vm ec (PlutusRunnable rs) =
    PV3.evaluateScriptCounting (toMajorProtocolVersion pv) vm ec rs
      . PV3.toData
      . unPlutusV3Args
  mkTermToEvaluate pv (PlutusRunnable rs) =
    P.mkTermToEvaluate P.PlutusV3 (toMajorProtocolVersion pv) rs
      . pure
      . PV3.toData
      . unPlutusV3Args

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

-- | Prevent decoding a version of Plutus until
-- the appropriate protocol version.
guardPlutus :: Language -> Decoder s ()
guardPlutus lang =
  let v = case lang of
        PlutusV1 -> natVersion @5
        PlutusV2 -> natVersion @7
        PlutusV3 -> natVersion @9
   in unlessDecoderVersionAtLeast v $
        fail (show lang <> " is not supported until " <> show v <> " major protocol version")

withSamePlutusLanguage ::
  forall f1 f2 l1 l2 a.
  (PlutusLanguage l1, PlutusLanguage l2) =>
  f1 l1 ->
  f2 l2 ->
  (forall l. PlutusLanguage l => f1 l -> f2 l -> a) ->
  Maybe a
withSamePlutusLanguage x1 x2 f = f x1 <$> gcast x2
