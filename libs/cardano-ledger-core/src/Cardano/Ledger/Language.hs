{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- | This module provides data structures and operations for talking about
--     Non-native Script languages. It is expected that new languages (or new
--     versions of old languages) will be added here.
module Cardano.Ledger.Language where

import Cardano.Ledger.Binary (FromCBOR (..), ToCBOR (..), decodeEnumBounded, encodeEnum)
import Cardano.Ledger.TreeDiff (ToExpr (..))
import Control.DeepSeq (NFData (..))
import Data.Ix (Ix)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

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
  deriving (Eq, Generic, Show, Ord, Enum, Bounded, Ix)

instance NoThunks Language

instance NFData Language

instance ToCBOR Language where
  toCBOR = encodeEnum

instance FromCBOR Language where
  fromCBOR = decodeEnumBounded

nonNativeLanguages :: [Language]
nonNativeLanguages = [minBound .. maxBound]

instance ToExpr Language

-- | Singleton for '@Language@'
data SLanguage (l :: Language) where
  SPlutusV1 :: SLanguage 'PlutusV1
  SPlutusV2 :: SLanguage 'PlutusV2

deriving instance Eq (SLanguage l)

deriving instance Show (SLanguage l)

instance
  forall (l :: Language).
  (Typeable l, IsLanguage l) =>
  ToCBOR (SLanguage l)
  where
  toCBOR = toCBOR . fromSLanguage

instance
  forall (l :: Language).
  (Typeable l, IsLanguage l) =>
  FromCBOR (SLanguage l)
  where
  fromCBOR = toSLanguage =<< fromCBOR @Language

-- | Reflection for '@SLanguage@'
fromSLanguage :: SLanguage l -> Language
fromSLanguage = \case
  SPlutusV1 -> PlutusV1
  SPlutusV2 -> PlutusV2

-- | For implicit reflection on '@SLanguage@'
-- See "Cardano.Ledger.Alonzo.TxInfo" for example usage
class IsLanguage l where
  isLanguage :: SLanguage l

instance IsLanguage 'PlutusV1 where
  isLanguage = SPlutusV1

instance IsLanguage 'PlutusV2 where
  isLanguage = SPlutusV2

toSLanguage :: forall (l :: Language) m. (IsLanguage l, MonadFail m) => Language -> m (SLanguage l)
toSLanguage lang
  | fromSLanguage thisLanguage == lang = pure thisLanguage
  | otherwise = fail $ "Plutus language mismatch. Expected " ++ show thisLanguage ++ ", but got: " ++ show lang
  where
    thisLanguage :: SLanguage l
    thisLanguage = isLanguage
