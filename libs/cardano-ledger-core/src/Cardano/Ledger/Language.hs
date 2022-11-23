{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module provides data structures and operations for talking about
--     Non-native Script languages. It is expected that new languages (or new
--     versions of old languages) will be added here.
module Cardano.Ledger.Language where

import Cardano.Ledger.Binary (FromCBOR (..), ToCBOR (..), decodeInt, invalidKey)
import Control.DeepSeq (NFData (..))
import Data.Ix (Ix)
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
  toCBOR PlutusV1 = toCBOR (0 :: Int)
  toCBOR PlutusV2 = toCBOR (1 :: Int)

instance FromCBOR Language where
  fromCBOR = do
    n <- decodeInt
    -- We need to check bounds in order to avoid partial cases of `toEnum`
    lang <-
      if n >= fromEnum (minBound :: Language) && n <= fromEnum (maxBound :: Language)
        then pure $ toEnum n
        else invalidKey (fromIntegral n)
    -- We pattern match on lang so that the type checker will
    -- notice when new language versions are added.
    pure $ case lang of
      PlutusV1 -> lang
      PlutusV2 -> lang

nonNativeLanguages :: [Language]
nonNativeLanguages = [minBound .. maxBound]
