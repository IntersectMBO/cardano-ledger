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
  BinaryPlutus (..),

  -- * Value level Plutus Language version
  Language (..),
  mkLanguageEnum,
  languageToText,
  languageFromText,
  nonNativeLanguages,
  guardPlutus,

  -- * Type level Plutus Language version
  SLanguage (..),
  IsLanguage (..),
  fromSLanguage,
  toSLanguage,
  withSLanguage,
) where

import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  Decoder,
  EncCBOR (..),
  FromCBOR,
  ToCBOR,
  decodeEnumBounded,
  encodeEnum,
  natVersion,
  unlessDecoderVersionAtLeast,
 )
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.TreeDiff (ToExpr (..))
import Control.DeepSeq (NFData (..), rwhnf)
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
import qualified Data.ByteString.Base64 as B64 (encode)
import Data.ByteString.Short (ShortByteString, fromShort)
import Data.Ix (Ix)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

data Plutus = Plutus
  { plutusLanguage :: !Language
  , plutusScript :: !BinaryPlutus
  }
  deriving stock (Eq, Ord, Show, Generic)

instance ToExpr Plutus

-- | Already in Normal Form
instance NFData Plutus where
  rnf = rwhnf

instance NoThunks Plutus

-- | Binary representation of a Plutus script.
newtype BinaryPlutus = BinaryPlutus {unBinaryPlutus :: ShortByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (ToCBOR, FromCBOR, EncCBOR, DecCBOR, NFData, NoThunks)

instance ToExpr BinaryPlutus

instance Show BinaryPlutus where
  show = show . B64.encode . fromShort . unBinaryPlutus

instance DecCBOR (Annotator BinaryPlutus) where
  decCBOR = pure <$> decCBOR

instance SafeToHash BinaryPlutus where
  originalBytes (BinaryPlutus binaryBlutus) = fromShort binaryBlutus

-- | Non-Native Script language. This is an Enumerated type.
-- This is expected to be an open type. We will add new Constuctors
-- to this type as additional Non-Native scripting language as are added.
-- We use an enumerated type for two reasons.
-- 1) We can write total functions by case analysis over the constructors
-- 2) We will use DataKinds to make some datatypes  indexed by Language
-- For now, the only Non-Native Scriting language is Plutus
-- We might add new languages in the futures.
--
-- Note that the the serialization of 'Language' depends on the ordering.
data Language
  = PlutusV1
  | PlutusV2
  | PlutusV3
  deriving (Eq, Generic, Show, Ord, Enum, Bounded, Ix)

instance NoThunks Language

instance NFData Language

-- | Make a language form its `Enum` index.
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

instance EncCBOR Language where
  encCBOR = encodeEnum

instance DecCBOR Language where
  decCBOR = decodeEnumBounded

nonNativeLanguages :: [Language]
nonNativeLanguages = [minBound .. maxBound]

instance ToExpr Language

-- | Singleton for '@Language@'
data SLanguage (l :: Language) where
  SPlutusV1 :: SLanguage 'PlutusV1
  SPlutusV2 :: SLanguage 'PlutusV2
  SPlutusV3 :: SLanguage 'PlutusV3

deriving instance Eq (SLanguage l)

deriving instance Show (SLanguage l)

instance IsLanguage l => EncCBOR (SLanguage l) where
  encCBOR = encCBOR . fromSLanguage

instance IsLanguage l => DecCBOR (SLanguage l) where
  decCBOR = toSLanguage =<< decCBOR @Language

-- | Reflection for '@SLanguage@'
fromSLanguage :: SLanguage l -> Language
fromSLanguage = \case
  SPlutusV1 -> PlutusV1
  SPlutusV2 -> PlutusV2
  SPlutusV3 -> PlutusV3

-- | For implicit reflection on '@SLanguage@'
-- See "Cardano.Ledger.Alonzo.TxInfo" for example usage
class Typeable l => IsLanguage (l :: Language) where
  isLanguage :: SLanguage l

instance IsLanguage 'PlutusV1 where
  isLanguage = SPlutusV1

instance IsLanguage 'PlutusV2 where
  isLanguage = SPlutusV2

instance IsLanguage 'PlutusV3 where
  isLanguage = SPlutusV3

toSLanguage :: forall l m. (IsLanguage l, MonadFail m) => Language -> m (SLanguage l)
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

withSLanguage :: Language -> (forall l. IsLanguage l => SLanguage l -> a) -> a
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
